import { Port, registerPorts } from './Ports'
import * as realm from 'realm-web'
import cuid from 'cuid'

const node = document.getElementById('app')

if (node === null) {
  throw new Error('No node with id "app" found')
}

if (!hasElm(window)) {
  throw new Error('No Elm app found')
}

const app = window.Elm.Main.init({
  flags: {
    languages: [],
    url: window.location.href,
    clientId: cuid()
  },
  node
})

const realmApp = new realm.App({ id: 'teo-web-ogsac' })

registerPorts(app.ports, realmApp)

interface WithElm {
  Elm: {
    Main: {
      init: (options: { flags: unknown, node: HTMLElement }) => {
        ports: Record<string, Port>
      }
    }
  }
}

function hasElm (w: Window & unknown): w is (Window & WithElm) {
  return Object.prototype.hasOwnProperty.call(w, 'Elm')
}
