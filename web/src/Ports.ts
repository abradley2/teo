import realm from 'realm-web'
import * as rx from 'rxjs'
import * as io from 'io-ts'

export function registerPorts (ports: Record<string, Port>, realmApp: realm.App): void {
  const $realmJwtPort = fromPort(
    'realmJwt',
    io.string
  )

  const $logErrorPort = fromPort(
    'logError',
    io.string
  )

  const requestParticipatingEventsPayload = io.type({
    tag: io.string,
    userId: io.string
  })

  type RequestParticipatingEventsPayload = io.TypeOf<typeof requestParticipatingEventsPayload>

  const $requestParticipatingEventsPort = fromPort(
    'requestParticipatingEvents',
    requestParticipatingEventsPayload
  )

  const requestHostingEventsPayload = io.type({
    tag: io.string,
    userId: io.string
  })

  type RequestHostingEventsPayload = io.TypeOf<typeof requestHostingEventsPayload>

  const $requestHostingEventsPort = fromPort(
    'requestHostingEvents',
    requestHostingEventsPayload
  )

  const $requestDataPort = fromPort(
    'requestData',
    io.string
  )

  const storeDataPayload = io.type({
    key: io.string,
    value: io.unknown
  })

  type StoreDataPayload = io.TypeOf<typeof storeDataPayload>

  const $storeDataPort = fromPort(
    'storeData',
    storeDataPayload
  )

  const $replaceUrlPort = fromPort(
    'replaceUrl',
    io.string
  )

  const $realmUser: rx.Observable<realm.User> = rx.mergeMap((jwt: string) => {
    return rx.from(realmApp.logIn(Realm.Credentials.jwt(jwt)))
  })($realmJwtPort)

  const $linkClicked: rx.Observable<[HTMLElement, MouseEvent]> = new rx.Observable((subscriber) => {
    document.addEventListener('click', function (e) {
      let node = e.target
      while (node !== null && node !== document && node !== document.body && node instanceof HTMLElement) {
        if (node.tagName === 'A' && node.getAttribute('data-link') !== null) {
          subscriber.next([node, e])
          return
        }
        node = node.parentNode
      }
    })
  })

  const $eventsCollection = rx.map((realmUser: realm.User) => {
    return realmUser.mongoClient('mongodb-atlas').db('teo').collection('events')
  })($realmUser)

  $linkClicked.subscribe(([node, e]) => {
    e.preventDefault()
    if (node.getAttribute('data-link') === 'replaceState') {
      window.history.replaceState(null, document.title, node.getAttribute('href'))
    } else {
      window.history.pushState(null, document.title, node.getAttribute('href'))
    }
    ports.linkClicked.send(window.location.href)
  })

  rx.mergeMap((
    [{ userId, tag }, eventsCollection]: [RequestParticipatingEventsPayload, Collection]
  ) => {
    console.log(eventsCollection)
    return rx.from(
      eventsCollection.find({
        participants: {
          $elemMatch: {
            $eq: userId
          }
        }
      })
        .then(convertBson)
        .then(tagResponse(tag))
    )
  })(
    rx.combineLatest([
      $requestParticipatingEventsPort,
      $eventsCollection
    ])
  )
    .subscribe((response) => {
      ports.requestParticipatingEventsResponse.send(response)
    })

  rx.mergeMap((
    [{ userId, tag }, eventsCollection]: [RequestHostingEventsPayload, Collection]
  ) => {
    return rx.from(
      eventsCollection.find({ userId })
        .then(convertBson)
        .then(tagResponse(tag))
    )
  })(
    rx.combineLatest([
      $requestHostingEventsPort,
      $eventsCollection
    ])
  )
    .subscribe((response) => {
      ports.requestHostingEventsResponse.send(response)
    })

  $logErrorPort.subscribe((error) => {
    console.error(error)
  })

  $storeDataPort.subscribe(({ key, value }: StoreDataPayload) => {
    window.localStorage.setItem(key, JSON.stringify(value))
  })

  $replaceUrlPort.subscribe((url) => {
    window.history.replaceState(null, document.title, url)
    requestAnimationFrame(() => {
      ports.linkClicked.send(window.location.href)
    })
  })

  rx.map((key: string) => {
    return [key, localStorage.getItem(key)]
  })($requestDataPort)
    .subscribe(([key, data]) => {
      console.log('SENDING ', data)
      ports.receiveData.send([key, data])
    })

  function fromPort<T> (port: string, decoder: io.Decoder<unknown, T>): rx.Observable<T> {
    if (typeof ports[port] === 'undefined') throw new Error(`Port ${port} not found`)
    return new rx.Observable((subscriber) => {
      ports[port].subscribe((data: unknown) => {
        const decoded = decoder.decode(data)
        switch (decoded._tag) {
          case 'Left':
            subscriber.error(decoded.left)
            break
          case 'Right':
            subscriber.next(decoded.right)
            break
        }
      })
    })
  }
}

function tagResponse <Response> (tag: string) {
  return function (response: Response) {
    return {
      status: 'Success',
      tag,
      response
    }
  }
}

function convertBson (obj: unknown): unknown {
  return JSON.parse(JSON.stringify(obj))
}

type Collection = Realm.Services.MongoDB.MongoDBCollection<Realm.Services.MongoDB.Document>

export interface Port {
  subscribe: (callback: (data: unknown) => void) => void
  send: (data: unknown) => void
}
