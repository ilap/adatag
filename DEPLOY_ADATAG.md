
## Deploy @adatag contract

0. Clone the repository and change directory to the workspce root

``` bash
  $ git clone https://github.com/ilap/adatag
  $ cd adatag
```

1. If the aiken contract is changed create a commit and bump [its version](contracts/aiken/aiken.toml) and create a new tag. Example:

``` bash
$ grep version contracts/aiken/{project.json,aiken.toml} | head -2
contracts/aiken/project.json:  "version": "0.2.0",
contracts/aiken/aiken.toml:version = "0.2.0"
$ git commit -m 'chore: Bumped aiken to v0.2.0'
$ git push origin main
$ git tag aiken/v0.2.0
$ git push origin aiken/v0.2.0
```

2. Create a new `.env`   file
``` bash
$ echo '
NETWORK=Preprod
ENVIRONMENT=Integration
PROVIDER=Kupmios
' > .env.preview
```

> **Note: this is for only preview or preprod deployments.**

3. Configure the [API urls](./libs/common/src/config/api-urls.json).

This PoC is a PWA that uses some public backends' endpoints, therefore it can be abused by any adversarial parties. If the community decides to develop further some mitigations are rquired for prevent any abuse of the system.

4. Optional: Manually configure the [genesis-params.json](./libs/common/src/config/genesis-params.json). 

Especially, the collector address for collecting the donations.

5. Configure the [Test user's seed](./libs/common/src/config/keys/test-users-seed.json)

``` bash
# Example
$ cat << EOF > ./libs/common/src/config/keys/test-users-seed.json
{
  "deployer": {
    "seed": "<DEPLOYER SEED>"
  },
  "collector": {
    "seed": "<YOUR COLLECTOR SEED>"
  },
  "user"<USER SEED>"
  }
}
EOF
```

> **Note: this is for only preview or preprod deployments. For mainnet a very dumb web based deploy page should be developed that use hardware wallets, or add HW wallett support for CLI based deployment.**



6. Run the deploy script

This deploys and rebuilds (if necessary) the plutus scripts, and create the [genesis-config-preview.json]](./apps/deploy/config/genesis-config-preview.json) file, that the PWA can use.

``` bash
$ cd adatag
$ nx run @adatag/deploy:deploy:preview
   ✔  4/4 dependent project tasks succeeded [3 read from cache]

   Hint: you can run the command with --verbose to see the full dependent project outputs

———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————


> nx run @adatag/deploy:cors-proxy:preview

> echo 'No CORS Proxy server is required'

No CORS Proxy server is required

> nx run @adatag/deploy:deploy:preview

> bun run src/cli/deploy.ts

PARAMS: {
  "hashAlg": "SHA2_256",
  "collectorAddress": "addr_test1qr2unerpwgg79lmu0lgp6r0f4jpwtwupm3d9u944nygcv630tqj8uphr57f2xeylny36dy6d2mh32z44hww3tkeg0ljsjlzakw",
  "collectionTime": 2,
  "depositBase": 1750,
  "deactivationTime": 1,
  "lockingDays": 0.04,
  "adahandle": "f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a"
}
Genesis config (Preview) is saved to: ./config/genesis-config-preview.json
##### BD: {
  "network": "Preview",
  "hashAlg": "SHA2_256",
  "genesisTransaction": "857546cc291d05c0b61570252dd54450f95f2eaf72b97d612b25c5e4e712b649",
  "bootstrapTime": {
    "epoch": 1721551665304,
    "date": "Sun Jul 21 2024 10:47:45 GMT+0200 (Central European Summer Time)"
  },
  "referenceScript": {
    "scriptAddress": "addr_test1wqh89j4q7e8ud7wd2wh8sy25lwa3xhrkyhcsln0kqdtxhjgr08ngw",
    "scriptHash": "2e72caa0f64fc6f9cd53ae781154fbbb135c7625f10fcdf603566bc9"
  },
  "authTokenScript": {
    "policyId": "bd8b24e94d672407394b6c24367d6da688135d941b1fca97b1ab69a8",
    "params": {
      "genesis_utxo": {
        "txHash": "a804f75abccf607921e9020f476c8dd7cb1f3f5da21d6c0a1bcc67f49b2cdeca",
        "outputIndex": 0
      }
    }
  },
  "timelockScript": {
    "scriptHash": "05023c0423136ed3303621e1144408359ff47d6738a9933aee15ef90",
    "scriptAddress": "addr_test1wqzsy0qyyvfka5esxcs7z9zypq6elaravuu2nye6ac27lyqsl6vdu",
    "params": {
      "collectorAddr": "addr_test1qr2unerpwgg79lmu0lgp6r0f4jpwtwupm3d9u944nygcv630tqj8uphr57f2xeylny36dy6d2mh32z44hww3tkeg0ljsjlzakw",
      "collectionTime": {
        "epoch": 1721724465304,
        "date": "Tue Jul 23 2024 10:47:45 GMT+0200 (Central European Summer Time)"
      }
    },
    "refIndex": 2
  },
  "stateholderScript": {
    "scriptHash": "531360875c6d3ef1ecf0bee963a9256f8ca9334682820e854fe46a60",
    "scriptAddress": "addr_test1wpf3xcy8t3knau0v7zlwjcafy4hce2fng6pgyr59fljx5cqywue2q",
    "params": {
      "authToken": "bd8b24e94d672407394b6c24367d6da688135d941b1fca97b1ab69a8"
    },
    "refIndex": 0
  },
  "adatagMinting": {
    "policyId": "495f290f868fab7189a4a7462d899c7976855850dd78a9fc221a718c",
    "params": {
      "lockingDays": {
        "days": 0.04,
        "ms": 3456000
      },
      "deactivationTime": {
        "epoch": 1721638065304,
        "date": "Mon Jul 22 2024 10:47:45 GMT+0200 (Central European Summer Time)"
      },
      "depositBase": 1750,
      "adahandle": "f0ff48bbb7bbe9d59a40f1ce90e9e9d0ff5002ec48f232b49ca0fb9a"
    },
    "refIndex": 1
  }
}

———————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————

 NX   Successfully ran target deploy for project @adatag/deploy and 9 tasks it depends on (43s)
```

## Deploy the Vite + React + TailwindCSS based PoC for adatag.io
