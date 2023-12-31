import { applyParamsToScript, Data, Validator } from "translucent-cardano";

type Proof =
  | {
      HashNode: { hash: string; left: Proof; right: Proof };
    }
  | {
      NodeHash: { hash: string };
    };

export interface AdatagAdatagMinting {
  new (params: {
    authToken: string;
    stateHolder: string;
    timedepositValidator: string;
    deactivationTime: bigint;
    depositBase: bigint;
    lockingDays: bigint;
    adahandle: string;
  }): Validator;
  rdmr:
    | {
        Minting: [
          {
            updateVal: { xi: string; xa: string; xb: string };
            appendVal: { xi: string; xa: string; xb: string };
            proof: Proof;
          },
        ];
      }
    | "Burning";
}

export const AdatagAdatagMinting = Object.assign(
  function (params: {
    authToken: string;
    stateHolder: string;
    timedepositValidator: string;
    deactivationTime: bigint;
    depositBase: bigint;
    lockingDays: bigint;
    adahandle: string;
  }) {
    return {
      type: "PlutusV2",
      script: applyParamsToScript(
        "590bf601000032323232323232323222232533300732323232533300b3370e900018050008991919191919191919191919191919191919192999810981200109919299981019b8748010c07c0044c8c8c8c8c8c8c94ccc0a8c0b40084c94ccc0a0cdc3a4008604e0022646464646464a66605c66e1d20000011323253330303370e0089001099192999819192999819991919299981b19b8748008c0d40044c8c8c94ccc0e400840045281919299981d19b87480000045280a99981d19b87480080044c8c94ccc0f0cdc3a4004607a6ea8c088c0e8c088c0e80204cdc4803000899b88006001375a608000260700042944c0e0004c07cc0d8c078c0d8010c8c94ccc0e4cdc3a4000002294454ccc0e4cdc3a400400226464a66607666e1d2002303c3754604260726044607200e266e240040144cdc40008029bad303f001303700214a0606e002603c606a603c606a0066eb4c0ecc0d000458c070c0ccc070c0cc074cdd2a40006607066e952000330383374a90011981c1ba80014bd701981c19981a2514c0103d87a80004c0103d87980004bd701981c26010ad8799fd87b80d87a80ff004bd701bad3003303103014a22a666066646464a66606c66e1d200200114a02944c0d0004c94ccc0e0004530103d87a800013374a90001981c99981a99b8748008c0d8dd5181d000a6103d87a80004c0103d87980004bd70191980080081211299981c8008a5eb804c8c8c8c94ccc0e8cdc3a400400226600c00c00626607c66607466e1d2002303b3754607e607000498103d87a80004c0103d87980003300600600330380013253330393371090001998070008030038a60107d8799fd87a80ff0014c103d87a80003756603c606c004607a00460760026eb8c0e0c0e4c0e4c0e4c0e4c0e4c0e4c0c40c05288991919299981c981e001099299981b99b8748010c0d80044c8c8c8c8c94ccc0f0cc894ccc0f8cdc3a4004002266e2400800c54ccc0f8cdc3a4008002266e24cdc1801240080062a66607c66e1d200600113371266e0c00920080031533303e3370e9004000899b893370600490080018a99981f19b87480280044cdc499b830024808000c54ccc0f8cdc3a4018002266e24cdc180124080006266e25200a003375a601660740726e340284c94ccc0f4cdc3a40046078002266e24cdc01bad3042303b001375a60846086608660866086608660760740062c60466074604460740482940ccc040dd59810981c80324500488100375a6040607000464a66607466e1d200000113232323253330413044002149858dd6982100098210011bae30400013038002163038001303d0013035001163018303400116303a00133010023001375c602a60620606eb8c050c0c00244cccccccc8c8c8c8c88888888c8c8c94ccc104cccccc02d280040020018010008999999805a5100700400300200114a0a666080a66608066ebccc02cdd71813181f002804a6103d879800013375e660160126eb8c088c0f801530103d879800014a0264646608c0046608ca66608466ebc01c018401c4018cc1180052f5c066e9520003304537520126608a6ea4028cc114c08cc0fc0192f5c066e952000330443027303e005330443026303e00533044375201297ae016300b003300a00322222232323232323253330425333042005100314a0266e3c00402c5281bae304600130460023370e900118201baa304400130440023370e9001181f1baa30420013233330010014a09400148888c94ccc104cdc3a400400226608a66608200898103d87a80004c0103d8798000330453330410034c0103d87a80004c0103d8798000330453046303f0024bd7009919191919191919191919191919191919191919191919299982c299982c01108008a5013232323232323232323233066333062533306201614a2202098103d87a80004c0103d879800033066333062533306201414a2201c98103d87a80004c0103d879800033066375266605e00a00400297ae0375c60c60046eb8c1840054ccc17ccdc7807a4520e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855001337606ea4004dd4804899bb0375201e6ea4004ccc0acc0a8009220120e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b85500488120e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b85500533305d016130290051302900330610013061002305f001305f01e305d01d13305c333058533305800c14a2200c98103d87a80004c0103d87980003305c333058533305800a14a2200898103d87a80004c0103d87980003305c375266604a02c01000497ae0533305701014a2201e6eb8c16c004c16c008cdc3a400460aa6ea8c164004c164008cdc3a400460a66ea8c15c004cccc05405402001c02cdd7182a800982a80119b8748008c13cdd51829800982980119b8748008c134dd518288009999807807801000803a9998250060a511003533304900c14a2200266e3c018038cdc780280718258009825801182480098248011bae3047001303f002303f00122533303733720004002298103d8798000153330373371e0040022980103d87a800014c103d87b800023003337146eb8c070c0cc004cdc51bae301b3033001375c602e6066002444600866e2800ccdc500100091b92001375c602860600126eb8c008c0c0024dd7180098180089bae3001303000930193030003301830300033014303000314a04606e60706070607060700024606c606e606e606e0022940c0d0004c0b00a854ccc0b8cdc38012400229405281816014999800991980080080c9129998188008a5eb7bdb1804c8c8c8c94ccc0c8cdc7a45000021003133036337606ea4008dd3000998030030019bab3033003375c6062004606a00460660020406eb8c038c0a800c888c8c8c94ccc0c4cdc3a40040022900009bad3036302f002302f0013253330303370e90010008a60103d87a8000132323300100100222533303600114c103d87a800013232323253330373371e014004266e9520003303b375000297ae0133006006003375a60700066eb8c0d8008c0e8008c0e0004dd5981a9817001181700099198008008021129998198008a6103d87a800013232323253330343371e010004266e95200033038374c00297ae01330060060033756606a0066eb8c0cc008c0dc008c0d4004c020004c0b8004c09800458c024c09400458c0ac004cc004050dd718059811811111980580111919299981499b87480080044cdc78021bae302e302700214a0604e002601c604a601c604a0026002004464a66604866e1d2000001132323232323232323232323253330333036002132498c94ccc0c4cdc3a40000022a666068605e0142930b0a99981899b874800800454ccc0d0c0bc0285261616302f00916375c606800260680046eb8c0c8004c0c8008dd7181800098180011bae302e001302e002302c001302c002375a605400260440042c6044002604c002603c0022c6002603a600a603a00446048604a604a0022c6044002646600401e46464a66604066e1d200200113371e0086eb8c094c078008528180f0009802980e1802980e1802180e0009bae3002301a01922323300100100322533302200114bd70099192999810980280109981280119802002000899802002000981300118120009181018108009180f800980e800980e800980e000980d8011bab3019001301900130180023758602c002602c002602a0046eb0c04c004c02c014dd7180880098048008b18078009807801180680098028010a4c26cac64646464a66601466e1d200000113232533300f3012002132498c94ccc034cdc3a40000022646464646464a66602c60320042646464931807001980600218058028b180b800980b801180a800980a801180980098058010b18058008b180800098040030a99980519b874800800454ccc034c02001852616163008005232533300a3370e900000089919191919192999809980b0010a4c2c6eb8c050004c050008dd7180900098090011bae301000130080021630080013001001223253330093370e900000089919191919192999809180a8010991924c66014014004660120120062c60260026026004602200260220046eb8c03c004c01c00854ccc024cdc3a400400226464a66601c60220042930b1bae300f0013007002163007001230053754002460066ea80055cd2ab9d5573caae7d5d02ba157441",
        [params],
        {
          dataType: "list",
          items: [
            {
              title: "MintParams",
              anyOf: [
                {
                  title: "MintParams",
                  dataType: "constructor",
                  index: 0,
                  fields: [
                    { dataType: "bytes", title: "authToken" },
                    { dataType: "bytes", title: "stateHolder" },
                    { dataType: "bytes", title: "timedepositValidator" },
                    { dataType: "integer", title: "deactivationTime" },
                    { dataType: "integer", title: "depositBase" },
                    { dataType: "integer", title: "lockingDays" },
                    { dataType: "bytes", title: "adahandle" },
                  ],
                },
              ],
            },
          ],
        } as any,
      ),
    };
  },
  {
    rdmr: {
      title: "MintAction",
      anyOf: [
        {
          title: "Minting",
          dataType: "constructor",
          index: 0,
          fields: [
            {
              anyOf: [
                {
                  title: "MintRedeemer",
                  dataType: "constructor",
                  index: 0,
                  fields: [
                    {
                      title: "updateVal",
                      anyOf: [
                        {
                          title: "Val",
                          dataType: "constructor",
                          index: 0,
                          fields: [
                            { dataType: "bytes", title: "xi" },
                            {
                              dataType: "bytes",
                              title: "xa",
                            },
                            { dataType: "bytes", title: "xb" },
                          ],
                        },
                      ],
                    },
                    {
                      title: "appendVal",
                      anyOf: [
                        {
                          title: "Val",
                          dataType: "constructor",
                          index: 0,
                          fields: [
                            { dataType: "bytes", title: "xi" },
                            {
                              dataType: "bytes",
                              title: "xa",
                            },
                            { dataType: "bytes", title: "xb" },
                          ],
                        },
                      ],
                    },
                    {
                      title: "proof",
                      anyOf: [
                        {
                          title: "HashNode",
                          dataType: "constructor",
                          index: 0,
                          fields: [
                            { dataType: "bytes", title: "hash" },
                            {
                              title: "left",
                              $ref: "#/definitions/ilap~1labeled_tree~1types~1Proof",
                            },
                            {
                              title: "right",
                              $ref: "#/definitions/ilap~1labeled_tree~1types~1Proof",
                            },
                          ],
                        },
                        {
                          title: "NodeHash",
                          dataType: "constructor",
                          index: 1,
                          fields: [{ dataType: "bytes", title: "hash" }],
                        },
                      ],
                    },
                  ],
                },
              ],
            },
          ],
        },
        {
          title: "Burning",
          dataType: "constructor",
          index: 1,
          fields: [],
        },
      ],
    },
  },
) as unknown as AdatagAdatagMinting;

export interface AlwaysFailAlwaysFail {
  new (): Validator;
  _d: undefined;
  _r: undefined;
}

export const AlwaysFailAlwaysFail = Object.assign(
  function () {
    return { type: "PlutusV2", script: "510100003222253330044a029309b2b2b9a1" };
  },
  {
    _d: {
      title: "Unit",
      description: "The nullary constructor.",
      anyOf: [{ dataType: "constructor", index: 0, fields: [] }],
    },
  },
  {
    _r: {
      title: "Unit",
      description: "The nullary constructor.",
      anyOf: [{ dataType: "constructor", index: 0, fields: [] }],
    },
  },
) as unknown as AlwaysFailAlwaysFail;

export interface AlwaysMintMint {
  new (): Validator;
  _r: undefined;
}

export const AlwaysMintMint = Object.assign(
  function () {
    return { type: "PlutusV2", script: "51010000322253330034a229309b2b2b9a01" };
  },
  {
    _r: {
      title: "Unit",
      description: "The nullary constructor.",
      anyOf: [{ dataType: "constructor", index: 0, fields: [] }],
    },
  },
) as unknown as AlwaysMintMint;

export interface OneshotAuthToken {
  new (utxoRef: {
    transactionId: { hash: string };
    outputIndex: bigint;
  }): Validator;
  _r: undefined;
}

export const OneshotAuthToken = Object.assign(
  function (utxoRef: { transactionId: { hash: string }; outputIndex: bigint }) {
    return {
      type: "PlutusV2",
      script: applyParamsToScript(
        "58af0100003232323232323232322225333006323232323232533300c3370e9000180580189919299980719b8748000c0340044c92898060008b19198008008019129998090008a60103d87a80001323253330113375e602c601e00401c266e952000330150024bd70099802002000980b001180a00098050018b1bac30100013008003300e001300e002300c001300400114984d9588c014dd5000918019baa0015734aae7555cf2ab9f5740ae855d101",
        [utxoRef],
        {
          dataType: "list",
          items: [
            {
              title: "OutputReference",
              description:
                "An `OutputReference` is a unique reference to an output on-chain. The `output_index`\n corresponds to the position in the output list of the transaction (identified by its id)\n that produced that output",
              anyOf: [
                {
                  title: "OutputReference",
                  dataType: "constructor",
                  index: 0,
                  fields: [
                    {
                      title: "transactionId",
                      description:
                        "A unique transaction identifier, as the hash of a transaction body. Note that the transaction id\n isn't a direct hash of the `Transaction` as visible on-chain. Rather, they correspond to hash\n digests of transaction body as they are serialized on the network.",
                      anyOf: [
                        {
                          title: "TransactionId",
                          dataType: "constructor",
                          index: 0,
                          fields: [{ dataType: "bytes", title: "hash" }],
                        },
                      ],
                    },
                    { dataType: "integer", title: "outputIndex" },
                  ],
                },
              ],
            },
          ],
        } as any,
      ),
    };
  },
  {
    _r: {
      title: "Unit",
      description: "The nullary constructor.",
      anyOf: [{ dataType: "constructor", index: 0, fields: [] }],
    },
  },
) as unknown as OneshotAuthToken;

export interface StateHolderStateHolder {
  new (authToken: string): Validator;
  oldState: {
    operationCount: bigint;
    operation: "AdatagAdded" | "AdatagRemoved";
    adatag: string;
    size: string;
    rootHash: string;
    mintingPolicy: string;
  };
  _r: Data;
}

export const StateHolderStateHolder = Object.assign(
  function (authToken: string) {
    return {
      type: "PlutusV2",
      script: applyParamsToScript(
        "59045c0100003232323232323232322322223232533300a32323232533300e3370e900118068008991919191919191919191919299980d19b8748000c0640044c8c8c8c8c8c8c94ccc090c09c0084c8c94ccc08ccdc3a400860440022646464646464a66605266446666006646600200202a44a666060002297adef6c6013232323253330313371e9101000021003133035337606ea4008dd3000998030030019bab3032003375c606000460680046064002004002a66605600a290010a40026eb8c008c09c014dd71804181380289929998151999801000813199b8c480012002375c6012605000c90010a99981519b87332300100122533302f0011480004cdc024004660040046064002646600200200444a66605e002297ae0133030302d3031001330020023032001480104c8c8c8c94ccc0b8cdc39bad3016302c00a337006eb4c058c0b00a520021533302e3371290000008a99981719b8700133700004a66605c010290010a4002266e3cdd7180398160149bae3007302c00a14a0294052818011bae3003302b0093001375c6004605404e46464a66605c66e1c00920601533302e3370e00290010a40002c26464666002002900024000444a66606466e1c00401040084ccc00c00cc94ccc0cd4ccc0cccdc4a40c0002266e24005207214a0266e00cdc1001a402866e040052060163371c00e00266e000052002371a0066e34008cdc7000a4000460606062606260620022c2c6eacc034c09c024588888c8c94ccc0c4c0d00084c8c94ccc0c0cdc7801003099b8700100514a06eb4c0c4008dd718178008b18190009919299981719b874800800452f5bded8c026eacc0ccc0b0008c0b0004c8cc004004014894ccc0c40045300103d87a800013232323253330323371e012004266e95200033036374c00297ae0133006006003375660660066eb8c0c4008c0d4008c0cc0048c0b4c0b8c0b8c0b8c0b8c0b8004cdd79805181200126103d8798000301d0013029001302100116300130200022302730283028001163025001323300100100c22533302400114bd7009919299981199baf300b302100200513302700233004004001133004004001302800230260013023001301b3001301b0022302230230013020001301800116323300100100922533301e00114c103d87a800013232533301d3375e600a603600401c266e952000330210024bd70099802002000981100118100009180f0009bab301c001301c001301b00237586032002603200260300046eb0c058004c038014c050004c03000458c048004c048008c040004c02000c526136563001004232533300a3370e90000008991919191919191919191919299980c980e00109924c64a66602e66e1d20000011533301a301500a14985854ccc05ccdc3a40040022a666034602a0142930b0b180a8048b1bae301a001301a002375c603000260300046eb8c058004c058008dd7180a000980a001180900098090011bad30100013008002163008001375c0024600a6ea80048c00cdd5000ab9a5573aaae7955cfaba05742ae881",
        [authToken],
        { dataType: "list", items: [{ dataType: "bytes" }] } as any,
      ),
    };
  },
  {
    oldState: {
      title: "TreeState",
      anyOf: [
        {
          title: "TreeState",
          dataType: "constructor",
          index: 0,
          fields: [
            { dataType: "integer", title: "operationCount" },
            {
              title: "operation",
              anyOf: [
                {
                  title: "AdatagAdded",
                  dataType: "constructor",
                  index: 0,
                  fields: [],
                },
                {
                  title: "AdatagRemoved",
                  dataType: "constructor",
                  index: 1,
                  fields: [],
                },
              ],
            },
            { dataType: "bytes", title: "adatag" },
            { dataType: "bytes", title: "size" },
            { dataType: "bytes", title: "rootHash" },
            { dataType: "bytes", title: "mintingPolicy" },
          ],
        },
      ],
    },
  },
  { _r: { title: "Data", description: "Any Plutus data." } },
) as unknown as StateHolderStateHolder;

export interface TimeDepositTimedeposit {
  new (params: { collector: string; collectionTime: bigint }): Validator;
  datum: Data;
  rdmr: "Collect" | "Redeem";
}

export const TimeDepositTimedeposit = Object.assign(
  function (params: { collector: string; collectionTime: bigint }) {
    return {
      type: "PlutusV2",
      script: applyParamsToScript(
        "59054b0100003232323232323232323232222232533300832323232533300c3370e9000002099999800a51008375c600460140126eb4c00cc0280240184c94ccc034cdc3a40006018002264646464a66602a603000426666600c94003400c00402c58dd6980b000980b0011bae3014001300b0011632323232323253301337326600201c910100100e3001001222533333301b002132323232330095333017337100069007099b80483c80400c54ccc05ccdc4001a410004266e04cdc0241002800690070b19b8a489012800001533301a001133714911035b5d2900004133714911035b5f2000375c603266600e00266ec1300102415d00375266e292210129000042233760980103422c2000375266601001000466e28dd7180d0009bae301b001375860300046eb4c058004c8cdd81ba83016001374e602e0026ea80084c94ccc0600044cdc5245027b7d00002133714911037b5f2000375c602e64646600200200644a66603600220062664466ec130103422c20003752666012012603600466e29221023a2000333009009301c002337146eb8c06c004dd7180e000980e80099801001180f00099bb04c10342207d0037520046eac0084c94ccc0600044cdc52441025b5d00002133714911035b5f2000375c602e66600a00266ec1300102415d0037520044466ec1300103422c2000375266600c00c00466e28dd7180c0009bae3019001375800426600a6eb40080044c8cdc524410268270033223233300100100300222253330193371000490000800899191919980300319b8100548008cdc599b80002533301c33710004900a0a40c02903719b8b33700002a66603866e2000520141481805206e0043370c004901019b8300148080cdc700300119b81371a002900119b8a4881012700002375c004444646600200200844a6660300022008266006603400266004004603600244646600200200644a66602466e1c0052000133714910101300000315333012337100029000099b8a489012d003300200233702900000089980299b8400148050cdc599b803370a002900a240c00066002002444a66601e66e2400920001001133300300333708004900a19b8b3370066e1400920144818000488888c8c94ccc048008400452819191980080080111299980c0008a501323253330163371e00401229444cc010010004c070008dd7180d0009bac301730183018301830183018301830183018300f3007300f00253330103375e98103d8798000004100513232323253330143370e900118098008991919299980b80108008a50323253330183370e90000008a50153330183370e900100089919299980d19b8748008c070dd51808980c1808980c004099b8900600113371000c0026eb4c07c004c058008528980b0009806180a1806980a0021919299980b99b87480000045288a99980b99b87480080044c8c94ccc064cdc3a400460366ea8c040c05cc03cc05c01c4cdc4800802899b88001005375a603c002602a0042940c054004c02cc04cc02cc04c00cdd6980d18090008b180498089804980880119ba548000cc05ccdd2a40006602e66e95200233017375000897ae0330173330124a298103d87a80004c0103d87980004bd701980ba6010ad8799fd87b80d87a80ff004bd70180b980c180c180c180c180c180c180c18079803980780119299980899b87480000044c8c8c8c94ccc064c07000852616375a603400260340046eb8c060004c03c01458c03c0108c0440048c040c044004c01c00c526136563253330083370e90000008a99980618030018a4c2c2a66601066e1d20020011533300c300600314985858c0180088c018dd5000918021baa0015734ae7155ceaab9e5573eae815d0aba25749",
        [params],
        {
          dataType: "list",
          items: [
            {
              title: "TimeDepositParams",
              anyOf: [
                {
                  title: "TimeDepositParams",
                  dataType: "constructor",
                  index: 0,
                  fields: [
                    { dataType: "bytes", title: "collector" },
                    {
                      dataType: "integer",
                      title: "collectionTime",
                    },
                  ],
                },
              ],
            },
          ],
        } as any,
      ),
    };
  },
  { datum: { title: "Data", description: "Any Plutus data." } },
  {
    rdmr: {
      title: "TimeDepositRedeemer",
      anyOf: [
        {
          title: "Collect",
          dataType: "constructor",
          index: 0,
          fields: [],
        },
        {
          title: "Redeem",
          dataType: "constructor",
          index: 1,
          fields: [],
        },
      ],
    },
  },
) as unknown as TimeDepositTimedeposit;