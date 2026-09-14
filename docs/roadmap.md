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

## Later: capability hub

`BoundOp` / `Api` is the capability. HTTP, OpenAPI, and MCP are host protocols / descriptions. Do not grow a second tool DSL.

- CLI as a host protocol: argv/subcommands invoke the same `BoundOp` via `OpArgs`. Humans get a CLI the way agents get MCP and browsers get HTTP.
- MCP client on `heddle.client` (Streamable HTTP, then stdio). Typed `callToolAs[In, Out]`.
- Resources and prompts as interpreters over `BoundOp` / dedicated values (GET-as-resource, prompt templates).
- MRTR (SEP-2322): `input_required`, elicitation, sampling, `roots/list`. Needed before tools can ask the client anything.
- Progress / log notifications on HTTP (`_meta.progressToken` → SSE) and on stdio (`notifications/progress`).
- Tasks extension (`io.modelcontextprotocol/tasks`).
- Skills over MCP, MCP Apps (capability + `_meta.ui` only; no iframe host).
- Official `@modelcontextprotocol/conformance` 2026-07-28 kit as a hard check.
- CIMD / DCR completeness on the OP; MCP authorization-code client flow.
- Catalog `invoke` for `inForm` / `inBytes` / `outSse` (HTTP-dispatch those; do not flatten into tools).
- OpenAPI `x-mcp-*` (or equivalent) so hints round-trip in the docs projection.
- Dual-era 2025-11-25 (`initialize`, `Mcp-Session-Id`, GET SSE) only if a real client we care about cannot speak 2026-07-28. Default remains latest-spec-only.

## Out

- Implicit access-token grants.
- Netty, JNI brotli, a JSON parser for user bodies in core.
- Requiring saferis (or File, or Memory) to compile `heddle-oauth`.
- Auto-promoting every REST operation as an MCP tool.
- A session store (`Mcp-Session-Id`) as the default MCP transport.
- Embedding a second authorization server inside `heddle-mcp`.
