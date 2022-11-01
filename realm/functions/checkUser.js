exports = function (user, args) {
    const root = args[0]
    return user.identities.some(function (identity) {
        return identity.id === root.userId && identity.provider_type === 'custom-token'
    })
}