API
===

### Get all utxos at wallet address
/wallet/{wallet-address}/utxos
```
[
    {
        "id": "1423856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1",
        "index" : 0,
        "amount": {
          "quantity" : 42000000,
          "unit"     : "lovelace"
        },
        "assets": [
            {
                "policy_id"  : "65ab82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b",
                "asset_name" : "coinA",
                "quantity"   : 123
            }
        ]
    }
]
```

### Get wallet balance
/wallet/{wallet-address}/balance
```
{
    "amount": {
          "quantity" : 42000000,
          "unit"     : "lovelace"
    },
    "assets": [
        {
            "policy_id"  : "65ab82542b0ca20391caaf66a4d4d7897d281f9c136cd3513136945b",
            "asset_name" : "coinA",
            "quantity"   : 123,
            "metadata": {
                "name": "SwaggerCoin",
                "description": "string",
                "ticker": "SWAG",
                "decimals": 2,
                "url": "http://example.com",
                "logo": "string"
            }
        }
    ]
}

```

### Submit Tx

/transaction/submit
```
{
 "type": "Tx AlonzoEra",
 "description": "",
 "cborHex": cborHex
}
```

Building docker image
=====================

```
nix-build release.nix -A mobile-wallet-backend
docker load < result
```