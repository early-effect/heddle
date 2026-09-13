# Heddle roadmap

Follow-up after the 0.1 feature-complete stack. Status: **Next**, **Later**, **Out**.

## Next: saferis provider stores

Published `heddle-oauth-saferis` (`oauth-saferis/`), on the root aggregate, depends on `heddle-oauth` + `rocks.earlyeffect::saferis`. `heddle-oauth` stays free of a SQL client.

Config (same key reserved in 0.1):

```
heddle.oauth.store = saferis
heddle.oauth.saferis.url = jdbc:postgresql://...
heddle.oauth.saferis.user = ...
heddle.oauth.saferis.password = ...
heddle.oauth.saferis.dialect = postgres | mysql | sqlite
```

`ProviderStores.saferis` implements all eight store traits (users, clients, authorization codes, tokens, devices, consents, OP sessions, PAR/JAR request URIs). DDL via saferis `Schema`. Do not invent a generic JDBC store.

Memory and File remain optional provided impls. Bringing your own `ProviderStores` is first-class.

## Later: public-readiness

- Specular docs (auth, oauth client, resource server, provider deploy, store config).
- First Central tag (`ZipxCentral` is already wired).
- Ascent / specular branches off `publishLocal`.

## Later: protocol and login

- OIDC conformance suite against `oauth/run`.
- Device authorization UI completion, PAR, JAR, JWE id_tokens, DCR, back-channel logout.
- CIBA, DPoP, FAPI 2.0 / JARM as a profile.
- `tls_client_auth` when the bind exposes a peer certificate.
- WebAuthn / passkeys as a `UserStore` authenticator.
- Admin UI for users/clients.
- File-backed JSON stores as the default for `oauth/run`.

## Later: HTTP

- HTTP/2 client (reuse the H2 codec).
- WebSocket client.
- Multi-range `multipart/byteranges`.

## Out

- Implicit access-token grants.
- Netty, JNI brotli, a JSON parser for user bodies in core.
- Requiring saferis (or File, or Memory) to compile `heddle-oauth`.
