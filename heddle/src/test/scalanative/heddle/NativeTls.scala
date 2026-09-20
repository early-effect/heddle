package heddle

private object NativeTls:
  val certPem: String =
    """-----BEGIN CERTIFICATE-----
      |MIICyTCCAbGgAwIBAgIJAOVgapgYQ16wMA0GCSqGSIb3DQEBCwUAMBQxEjAQBgNV
      |BAMMCWxvY2FsaG9zdDAeFw0yNjA5MjAxODA5MzlaFw0zNjA5MTcxODA5MzlaMBQx
      |EjAQBgNVBAMMCWxvY2FsaG9zdDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoC
      |ggEBAMZvHpdk6CnWufkN+R4c/U2/bvv0v44JQL0H+BVp1mjqV1mWRK1IPRSlYXmo
      |PeQ9ikQy7L347SSoCA+2KTaCOfC18y2nfJcZRKNUH41BiZeXVPmDZeE4A/1zrpur
      |1KGQsFhb+JsZl/6Y0cD5LrIsZ3VCxudgrmtbDESJT2/mwPn6iIGDefq08myuEXHp
      |kS3bWmldKwmRX0xFDg+iH3xboJH4yx1ZG1RrlH1MQpeh7f+ff4UcXvuRXm/7IxhA
      |ggPjSrop2JF9ax5gcP8vVuaSWESRSC/oVXVqxNeqjNLZDKuNXgn48d3SSYV2NXpH
      |DJkJAfjiTFfYKosE27ss1jj4rU8CAwEAAaMeMBwwGgYDVR0RBBMwEYIJbG9jYWxo
      |b3N0hwR/AAABMA0GCSqGSIb3DQEBCwUAA4IBAQBW6a7kImjHVo2HWZfGOdd9qeWX
      |bNkFKWqrKHGzNUeVpYfCPZT0fPhfBF288k4bYoNptyJY4/08bJmuiKLMy1abdi98
      |j7J35TV/NYiH6pI163yg3Ricgwwz+syROMP5KOApjeR75BTsuBr9RlNOvcfXjKNf
      |cvazmnZ5f0z/MJUwj/RM1/EzUKTe62vNXz39jst7MnfwmbRiMA7USJJ/q0+8kBfr
      |Y9KbddF9AfSlwQ7hvgcCIVqI+xh3wO62fiY6HCO7yMA7cNIzHsBwUFgYlspV3Yyy
      |hzIUUavLqQjEhy61s/ZgvbNfNAi4HxQIB1VDyt150b4ly3F3CnE1SKUIABLs
      |-----END CERTIFICATE-----
      |""".stripMargin

  val keyPem: String =
    """-----BEGIN PRIVATE KEY-----
      |MIIEvwIBADANBgkqhkiG9w0BAQEFAASCBKkwggSlAgEAAoIBAQDGbx6XZOgp1rn5
      |DfkeHP1Nv2779L+OCUC9B/gVadZo6ldZlkStSD0UpWF5qD3kPYpEMuy9+O0kqAgP
      |tik2gjnwtfMtp3yXGUSjVB+NQYmXl1T5g2XhOAP9c66bq9ShkLBYW/ibGZf+mNHA
      |+S6yLGd1QsbnYK5rWwxEiU9v5sD5+oiBg3n6tPJsrhFx6ZEt21ppXSsJkV9MRQ4P
      |oh98W6CR+MsdWRtUa5R9TEKXoe3/n3+FHF77kV5v+yMYQIID40q6KdiRfWseYHD/
      |L1bmklhEkUgv6FV1asTXqozS2QyrjV4J+PHd0kmFdjV6RwyZCQH44kxX2CqLBNu7
      |LNY4+K1PAgMBAAECggEANCtWsZrX5tgLQM1Jp38boWso9JjeG6uXF5uWv5p5wk+J
      |84WsPJp3ZIwuWlu/GdiMF24EC9X8Et0ScX8Eys3mCcDsVB5qhRchkoc1XF/UmsQw
      |lQDUsieV9PaK+2ZrmqZ9ll1nuO87pLXsv31Dp5hBAvUxcfdbI+JmEUbAWb0jExUx
      |7Rumdo3dT1k2R2PDhy4s60iBVmjEc3UeHBKOp4LB+ZtcoIpVyT6NHul9388IzhRL
      |P5IWt2lZbpYl9+68fEwUBySNqCqKpETsDeXZDc0X3/rydp5y0xtUocRNa2EqJ3B5
      |M7OFMdmBppwdLiCwxoUikbrNTpFeR/7K+/Fh+HPwUQKBgQD377fj9ghkPQCaSLKq
      |Ss2YwFZp5xrYtvu3KYNKLf2VEmTmnvChNUgNK9TjSeJ3SuXzpCKeVu0EJSVzAf2T
      |bPu/4BMEyut+Uucvfbz7Si/VgZBFNiTpj7lJEHLEK/xzIw65n6Gaw+NT9uVH0iUm
      |9z/DrEoLhhGEWYVUshzX0vHS+wKBgQDM40CF+AKkwIiQcKZw0BHirq5PwYdzacoL
      |UH4Pm22+DdYQ1Le5RggQ4TipIK9/TpNuZybEdEOWDSa+aJ9mmHD45oBsthMtByYq
      |jZJ3XtdyUCQRx8xBWeEISXouOXhJaTHglkAHpq+VTmnimVYBOERtl/Z+55te4qWM
      |hZhcJ1uevQKBgQCrlbfSyJNYI6uGGqejGK+edgWXtvuqXUBoqw8USC3Fe/xeakKn
      |nkMl8l6biadz3V60tbPLlubixn5bUFZYL8UuLfmbiH9fZipegItH8TiXbtoUO/th
      |tDiLaxmxz6sfV3S6W9IwVD6//g8BHFrf057KYTLBNOBskrOmQLmbV35J/wKBgQCS
      |EdYXbMhdstHpwBY5SW3m4Uhunfe2ZY5g4KLu942WuICMAUt2cCIh/p+JnD7iER/0
      |zt+JoaXpnTio+SfjWfz4xkR6vJgROw6PudzY86m/2rjMYFgTo0NWyCOuPtSt6axg
      |hF3j1odJd9zvawgw2G+YfoWC1hYj4IvMEhacZIbiNQKBgQDO5s2clMqW79GWyfLn
      |u7+7yPkSgYgOmlO5eDPeHDWclJyeyllK2m8hODV1lpy3hC3IcTCfi+0rbOESsq5n
      |NFEB1hegsDw+51ooux1a65ydWKSJiWPhR3F/1rEKMpQ1Jhh/M7tu6QGO0yPVJHjQ
      |z1iaQCW5rtpq+cpKfaa3rc1NvA==
      |-----END PRIVATE KEY-----
      |""".stripMargin
end NativeTls
