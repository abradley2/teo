exports = function (user, root) {
    return user.identities.some(function (identity) {
        return identity.id === root.userId && identity.provider_type === 'custom-token'
    })
}