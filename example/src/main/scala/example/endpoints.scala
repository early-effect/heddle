package example

import heddle.*

object Endpoints:
  def oauth(issuer: String): SecurityScheme =
    SecurityScheme.OAuth2(
      "oauth2",
      OAuthFlows(
        authorizationCode = Some(
          OAuthFlow(
            authorizationUrl = Some(s"$issuer/authorize"),
            tokenUrl = Some(s"$issuer/token"),
            scopes = Map("openid" -> "OpenID", "profile" -> "Profile"),
          )
        )
      ),
    )

  val listShows =
    Endpoint
      .get("shows")
      .out[List[Show]]
      .name("list_shows")
      .summary("What is on")
      .tag("shows")
      .hints(Hint.ReadOnly)

  val getShow =
    Endpoint
      .get("shows" / int("id"))
      .out[Show]
      .outError[NotFound](Status.NotFound)
      .name("get_show")
      .summary("One bill")
      .tag("shows")
      .hints(Hint.ReadOnly)

  val listSeats =
    Endpoint
      .get("shows" / int("id") / "seats")
      .out[List[String]]
      .outError[NotFound](Status.NotFound)
      .name("list_seats")
      .summary("Remaining seats")
      .tag("shows")
      .hints(Hint.ReadOnly)

  val pickup =
    Endpoint
      .get("pickup" / string("code"))
      .out[Order]
      .outError[NotFound](Status.NotFound)
      .name("pickup")
      .summary("Will-call lookup by pickup code")
      .tag("orders")
      .hints(Hint.ReadOnly)

  def createHold(issuer: String) =
    Endpoint
      .post("holds")
      .inJson[NewHold]
      .out[Hold](Status.Created)
      .outError[SeatingError](Status.Conflict)
      .name("create_hold")
      .summary("Reserve a contiguous block")
      .tag("orders")
      .auth(oauth(issuer))

  def createOrder(issuer: String) =
    Endpoint
      .post("orders")
      .inJson[NewOrder]
      .out[Order](Status.Created)
      .outError[NotFound](Status.NotFound)
      .name("create_order")
      .summary("Turn a hold into an order with a pickup code")
      .tag("orders")
      .auth(oauth(issuer))

  def seatTheParty(issuer: String) =
    Endpoint
      .post("parties")
      .inJson[Party]
      .out[PartySeated](Status.Created)
      .outError[SeatingError](Status.Conflict)
      .name("seat_the_party")
      .summary("Hold a contiguous block, price it, return a pickup code")
      .tag("orders")
      .hints(Hint.Destructive)
      .auth(oauth(issuer))

  val me =
    Endpoint
      .get("me")
      .out[Me]
      .auth(SecurityScheme.HttpBearer(bearerFormat = Some("JWT")))
      .summary("Current subject")
      .tag("auth")
end Endpoints
