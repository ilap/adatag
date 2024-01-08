/* eslint-disable @typescript-eslint/no-explicit-any */
import { applyParamsToScript, Data, Validator } from 'translucent-cardano'

export type Proof =
  | {
      HashNode: { hash: string; left: Proof; right: Proof }
    }
  | {
      NodeHash: { hash: string }
    }

export type MintRedeemer =
  | {
      Minting: [
        {
          updateVal: { xi: string; xa: string; xb: string }
          appendVal: { xi: string; xa: string; xb: string }
          proof: Proof
        },
      ]
    }
  | 'Burning'

export interface AdatagAdatagMinting {
  new (params: {
    authToken: string
    stateHolder: string
    timedepositValidator: string
    deactivationTime: bigint
    depositBase: bigint
    lockingDays: bigint
    adahandle: string
  }): Validator
  // It can be the MintRedeemer type too
  // MintRedeemer
  rdmr:
    | {
        Minting: [
          {
            updateVal: { xi: string; xa: string; xb: string }
            appendVal: { xi: string; xa: string; xb: string }
            proof: Proof
          },
        ]
      }
    | 'Burning'
}

export const AdatagAdatagMinting = Object.assign(
  function (params: {
    authToken: string
    stateHolder: string
    timedepositValidator: string
    deactivationTime: bigint
    depositBase: bigint
    lockingDays: bigint
    adahandle: string
  }) {
    return {
      type: 'PlutusV2',
      script: applyParamsToScript(
        '590eea010000323232323232323232323232323222232533300b32323232533300f3370e900018060008991919191919191919191919191919191919192999813181480109919299981219b8748010c0840044c8c8c8c8c8c8c94ccc0bcc0c80084c94ccc0b0cdc3a400860520022646464646464a66606466e1d20000011323253330343370e008900109919299981b19299981ba99981b991919299981d19b8748008c0dc0044c8c8c94ccc0f400840045281919299981f19b87480000045280a99981f19b87480080044c8c94ccc100cdc3a400460846ea8c088c0e8c088c0e80204cdc4803000899b88006001375a608a00260700042944c0e8004c07cc0d8c078c0d8010c8c94ccc0f4cdc3a4000002294454ccc0f4cdc3a400400226464a66607e66e1d200230413754604260726044607200e266e240040144cdc40008029bad3044001303700214a06072002603c606a603c606a0066eb4c100c0d000454cc0ec0e058c070c0ccc070c0cc074cdd2a40006607a66e9520003303d3374a90011981e9ba80014bd701981e99981c2514c103d87a80004c0103d87980004bd701981ea6010ad8799fd87b80d87a80ff004bd701bad3003303103014a22a660709213574696d655f656c61707365642872616e67652c20706172616d732e646561637469766174696f6e5f74696d6529203f2046616c73650014a0294454ccc0dd4ccc0dcc8c8c94ccc0e8cdc3a40040022940528981b00099299981e8008a6103d87a800013374a90001981f19981c99b8748008c0ecdd5181f800a6103d87a80004c0103d87980004bd70191980080081211299981f0008a5eb804c8c8c8c94ccc0f8cdc3a400400226600c00c00626608666607c66e1d2002304037546088607000498103d87a80004c0103d879800033006006003303a00132533303d3371090001998070008030038a60107d8799fd87a80ff0014c103d87a80003756603c606c004608400460800026eb8c0f4c0f8c0f8c0f8c0f8c0f8c0f8c0c40c05288a9981c24813b6f75747075745f6861735f746f6b656e286f7574707574732c20706172616d732e61646168616e646c652c2061646174616729203f2046616c73650014a0294454ccc0dcc8c8c94ccc0f8c1040084c94ccc0eccdc3a4008607000226464646464a666080a6660806644a66608466e1d20020011337120040062a66608466e1d200400113371266e0c0092004003153330423370e9003000899b893370600490040018a99982119b87480200044cdc499b830024804000c54ccc108cdc3a4014002266e24cdc1801240400062a66608466e1d200c00113371266e0c009204000313371290050019bad300b303a039371a014294454cc1052401516861735f76616c69645f6465706f73697428706172616d732e6465706f7369745f626173652c206465706f7369742c206279746561727261792e6c656e677468286164617461672929203f2046616c73650014a02a66608064a66608266e1d2002303e00113371266e00dd69823981d8009bad304730483048304830483048303b03a003153304203f163023303a3022303a02414a22a660829201336861735f76616c69645f646561646c696e6528706172616d732c2072616e67652c20646561646c696e6529203f2046616c73650014a02940ccc040dd59810981c803244100488100375a6040607000464a66607c66e1d200000113232323253330463049002149854cc10c0f858dd6982380098238011bae30450013038002153303f03916303a00130420013035001153303c0391630183034001153303b03616303f00133010023001375c602a6062060294454cc0e12401626861735f76616c69645f74696d656465706f7369745f6f757470757428706172616d732c2072616e67652c206f7574707574732c20706172616d732e74696d656465706f7369745f76616c696461746f722c2061646174616729203f2046616c73650014a06eb8c050c0c00244cccccccc8c8c8c8c88888888c8c8c94ccc114cccccc02d280040020018010008999999805a5100700400300200114a0a666088a66608866ebccc02cdd71813181f002804a60103d879800013375e660160126eb8c088c0f801530103d879800014a0264646609600466096a66608c66ebc01c018401c4018cc12c0052f5c066e9520003304a3752012660946ea4028cc128c08cc0fc0192f5c066e952000330493027303e005330493026303e00533049375201297ae016300b003300a00322222232323232323253330465333046005100314a0266e3c00402c5281bae304b001304b0023370e900118229baa304900130490023370e900118219baa30470013233330010014a09400148888c94ccc114cdc3a400400226609466608a00898103d87a80004c0103d87980003304a3330450034c0103d87a80004c0103d87980003304a304b303f0024bd7009919191919191919191919191919191919191919191919299982e299982e01108008a501323232323232323232323306b333066533306601614a2202098103d87a80004c0103d87980003306b333066533306601414a2201c98103d87a80004c0103d87980003306b375266605e00a00400297ae0375c60d00046eb8c1980054ccc18ccdc7807a4520e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855001337606ea4004dd4804899bb0375201e6ea4004ccc0acc0a8009220120e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b85500488120e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b8550053330610161302900513029003306600130660023064001306401e306201d13306133305c533305c00c14a2200c98103d87a80004c0103d87980003306133305c533305c00a14a2200898103d87a80004c0103d879800033061375266604a02c01000497ae0533305b01014a2201e6eb8c180004c180008cdc3a400460b46ea8c178004c178008cdc3a400460b06ea8c170004cccc05405402001c02cdd7182d000982d00119b8748008c150dd5182c000982c00119b8748008c148dd5182b0009999807807801000803a9998270060a511003533304d00c14a2200266e3c018038cdc780280718280009828001182700098270011bae304c001303f002304100122533303b33720004002298103d87980001533303b3371e0040022980103d87a800014c103d87b800023003337146eb8c070c0cc004cdc51bae301b3033001375c602e6066002444600866e2800ccdc500100091b92001375c602860600126eb8c008c0c0024dd7180098180089bae3001303000930193030003301830300033014303000314a046078607a607a607a607a002460766078607860780022940c0e4004c0b00a854ccc0c8cdc38012400229405281817014999800991980080080c91299981b0008a5eb7bdb1804c8c8c8c94ccc0d8cdc7a4500002100313303b337606ea4008dd3000998030030019bab3038003375c606c004607400460700020406eb8c038c0a800c888c8c8c94ccc0d4cdc3a40040022900009bad303b302f00230310013253330343370e90010008a60103d87a8000132323300100100222533303b00114c103d87a8000132323232533303b3371e014004266e95200033040375000297ae0133006006003375a607a0066eb8c0ec008c0fc008c0f4004dd5981d18170011818000991980080080211299981c0008a6103d87a800013232323253330383371e010004266e9520003303d374c00297ae0133006006003375660740066eb8c0e0008c0f0008c0e8004c020004c0cc004c09800454cc0b40a858c024c09400454cc0b009c58c0c0004cc004050dd718059811811111980580111919299981699b87480080044cdc78021bae3033302700214a06052002601c604a601c604a0026002004464a66605066e1d200000113232323232323232323232325333038303b002132498c94ccc0d4cdc3a40000022a666072605e0142930a9981b01a0b0a99981a99b874800800454ccc0e4c0bc0285261533036034161533036030163031009153303503016375c607200260720046eb8c0dc004c0dc008dd7181a800981a8011bae3033001303300230310013031002375a605e00260440042a660520462c60480026056002603c0022a6604a0442c6002603a600a603a00446052605460540022a6604603c2c604e002646600401e46464a66604866e1d200200113371e0086eb8c0a8c07800852818100009802980e1802980e1802180e0009bae3002301a01922323300100100322533302700114bd70099192999812980280109981500119802002000899802002000981580118148009181298130009181200098110009811000981080098100011bab301e001301e001301d00237586036002603600260340046eb0c060004c02c014dd7180b00098048008a998080068b180a000980a001180900098028010a4c2a6601892011856616c696461746f722072657475726e65642066616c7365001365632323232533300e3370e900000089919299980a180b80109924c64a66602266e1d20000011323232323232533301b301e0021323232498c03800cc030010c02c01454cc06004c58c070004c070008c068004c068008c060004c02c00854cc04803058c03400454cc04403058c054004c02001854ccc038cdc3a40040022a666024601000c2930a998078068b0a998078048b1805002919299980719b87480000044c8c8c8c8c8c94ccc060c06c008526153301501016375c603200260320046eb8c05c004c05c008dd7180a80098040010a998078048b180500098008009119299980699b87480000044c8c8c8c8c8c94ccc05cc0680084c8c9263300a00a00233009009003153301400f163018001301800230160013016002375c6028002600e0042a66601a66e1d20020011323253330133016002149854cc04002c58dd7180a00098038010a998070040b1804800918051baa00149128436f6e73747220696e646578206469646e2774206d61746368206120747970652076617269616e74004901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564002300637540029201244578706563746564206f6e20696e636f727265637420436f6e7374722076617269616e740049011d4578706563746564206e6f206669656c647320666f7220436f6e737472005734ae7155ceaab9e5573eae815d0aba21',
        [params],
        {
          dataType: 'list',
          items: [
            {
              title: 'MintParams',
              anyOf: [
                {
                  title: 'MintParams',
                  dataType: 'constructor',
                  index: 0,
                  fields: [
                    { dataType: 'bytes', title: 'authToken' },
                    { dataType: 'bytes', title: 'stateHolder' },
                    { dataType: 'bytes', title: 'timedepositValidator' },
                    { dataType: 'integer', title: 'deactivationTime' },
                    { dataType: 'integer', title: 'depositBase' },
                    { dataType: 'integer', title: 'lockingDays' },
                    { dataType: 'bytes', title: 'adahandle' },
                  ],
                },
              ],
            },
          ],
        } as any,
      ),
    }
  },

  {
    rdmr: {
      title: 'MintAction',
      anyOf: [
        {
          title: 'Minting',
          dataType: 'constructor',
          index: 0,
          fields: [
            {
              anyOf: [
                {
                  title: 'MintRedeemer',
                  dataType: 'constructor',
                  index: 0,
                  fields: [
                    {
                      title: 'updateVal',
                      anyOf: [
                        {
                          title: 'Val',
                          dataType: 'constructor',
                          index: 0,
                          fields: [
                            { dataType: 'bytes', title: 'xi' },
                            { dataType: 'bytes', title: 'xa' },
                            { dataType: 'bytes', title: 'xb' },
                          ],
                        },
                      ],
                    },
                    {
                      title: 'appendVal',
                      anyOf: [
                        {
                          title: 'Val',
                          dataType: 'constructor',
                          index: 0,
                          fields: [
                            { dataType: 'bytes', title: 'xi' },
                            { dataType: 'bytes', title: 'xa' },
                            { dataType: 'bytes', title: 'xb' },
                          ],
                        },
                      ],
                    },
                    {
                      title: 'proof',
                      anyOf: [
                        {
                          title: 'HashNode',
                          dataType: 'constructor',
                          index: 0,
                          fields: [
                            { dataType: 'bytes', title: 'hash' },
                            {
                              title: 'left',
                              $ref: '#/definitions/ilap~1labeled_tree~1types~1Proof',
                            },
                            {
                              title: 'right',
                              $ref: '#/definitions/ilap~1labeled_tree~1types~1Proof',
                            },
                          ],
                        },
                        {
                          title: 'NodeHash',
                          dataType: 'constructor',
                          index: 1,
                          fields: [{ dataType: 'bytes', title: 'hash' }],
                        },
                      ],
                    },
                  ],
                },
              ],
            },
          ],
        },
        { title: 'Burning', dataType: 'constructor', index: 1, fields: [] },
      ],
    },
  },
) as unknown as AdatagAdatagMinting

export interface AlwaysFailAlwaysFail {
  new (): Validator
  _d: undefined
  _r: undefined
}

export const AlwaysFailAlwaysFail = Object.assign(
  function () {
    return {
      type: 'PlutusV2',
      script:
        '5880010000323222253330045330054901455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b730014a02930a99802a4811856616c696461746f722072657475726e65642066616c736500136565734ae701',
    }
  },
  {
    _d: {
      title: 'Unit',
      description: 'The nullary constructor.',
      anyOf: [{ dataType: 'constructor', index: 0, fields: [] }],
    },
  },
  {
    _r: {
      title: 'Unit',
      description: 'The nullary constructor.',
      anyOf: [{ dataType: 'constructor', index: 0, fields: [] }],
    },
  },
) as unknown as AlwaysFailAlwaysFail

export interface AlwaysMintMint {
  new (): Validator
  _r: undefined
}

export const AlwaysMintMint = Object.assign(
  function () {
    return {
      type: 'PlutusV2',
      script:
        '583301000032322253330034a22930a9980224811856616c696461746f722072657475726e65642066616c736500136565734ae701',
    }
  },

  {
    _r: {
      title: 'Unit',
      description: 'The nullary constructor.',
      anyOf: [{ dataType: 'constructor', index: 0, fields: [] }],
    },
  },
) as unknown as AlwaysMintMint

export interface OneshotAuthToken {
  new (utxoRef: {
    transactionId: { hash: string }
    outputIndex: bigint
  }): Validator
  _r: undefined
}

export const OneshotAuthToken = Object.assign(
  function (utxoRef: { transactionId: { hash: string }; outputIndex: bigint }) {
    return {
      type: 'PlutusV2',
      script: applyParamsToScript(
        '59010401000032323232323232323232322225333007323232323232533300d3370e9000180600189919299980799b8748000c0380044c92898060008a998080068b191980080080191299980a0008a60103d87a80001323253330123375e6030601e00401c266e952000330170024bd70099802002000980c001180b00098050018a998070058b1bac3012001300800330100013010002300e0013004001149854cc02124011856616c696461746f722072657475726e65642066616c73650013656230073754002921244578706563746564206f6e20696e636f727265637420436f6e7374722076617269616e7400230043754002ae695ce2ab9d5573caae7d5d02ba157441',
        [utxoRef],
        {
          dataType: 'list',
          items: [
            {
              title: 'OutputReference',
              description:
                'An `OutputReference` is a unique reference to an output on-chain. The `output_index`\n corresponds to the position in the output list of the transaction (identified by its id)\n that produced that output',
              anyOf: [
                {
                  title: 'OutputReference',
                  dataType: 'constructor',
                  index: 0,
                  fields: [
                    {
                      title: 'transactionId',
                      description:
                        "A unique transaction identifier, as the hash of a transaction body. Note that the transaction id\n isn't a direct hash of the `Transaction` as visible on-chain. Rather, they correspond to hash\n digests of transaction body as they are serialized on the network.",
                      anyOf: [
                        {
                          title: 'TransactionId',
                          dataType: 'constructor',
                          index: 0,
                          fields: [{ dataType: 'bytes', title: 'hash' }],
                        },
                      ],
                    },
                    { dataType: 'integer', title: 'outputIndex' },
                  ],
                },
              ],
            },
          ],
        } as any,
      ),
    }
  },

  {
    _r: {
      title: 'Unit',
      description: 'The nullary constructor.',
      anyOf: [{ dataType: 'constructor', index: 0, fields: [] }],
    },
  },
) as unknown as OneshotAuthToken

export interface StateHolderStateHolder {
  new (authToken: string): Validator
  oldState: {
    operationCount: bigint
    operation: 'AdatagAdded' | 'AdatagRemoved'
    adatag: string
    size: string
    rootHash: string
    mintingPolicy: string
  }
  _r: Data
}

export const StateHolderStateHolder = Object.assign(
  function (authToken: string) {
    return {
      type: 'PlutusV2',
      script: applyParamsToScript(
        '59059e0100003232323232323232323232323232322322223232533300f3232323253330133370e900118080008991919191919191919191919299980f99b8748000c0700044c8c8c8c8c8c8c94ccc0a8c0b40084c8c94ccc0a0cdc3a4008604a0022646464646464a66605c66446666006646600200202a44a66606c002297adef6c6013232323253330363371e910100002100313303b337606ea4008dd3000998030030019bab3038003375c606c00460740046070002004002a66606000a290010a40026eb8c008c09c014dd71804181380289929998179999801000813199b8c480012002375c6012605000c90010a99981799b8733230010012253330350011480004cdc024004660040046070002646600200200444a66606a002297ae013303630333037001330020023038001480104c8c8c8c94ccc0cccdc39bad3016302c00a337006eb4c058c0b00a52002153330333371290000008a99981999b8700133700004a666066010290010a4002266e3cdd7180398160149bae3007302c00a14a0294052818011bae3003302b0093001375c6004605404e46464a66606666e1c0092060153330333370e00290010a40002c26464666002002900024000444a66606e66e1c00401040084ccc00c00cc94ccc0e14ccc0e0cdc4a40c0002266e24005207214a0266e00cdc1001a402866e0400520601533039033163371c00e00266e000052002371a0066e34008cdc7000a40004606c606e606e606e0022a660600542c2a660600542c6eacc034c09c02454cc0bc0a4588888c8c94ccc0dcc0e80084c8c94ccc0d4cdc7801003099b8700100514a06eb4c0dc008dd7181a8008a9981a0178b181c0009919299981999b874800800452f5bded8c026eacc0e4c0b0008c0bc004c8cc004004014894ccc0dc0045300103d87a800013232323253330373371e012004266e9520003303c374c00297ae0133006006003375660720066eb8c0dc008c0ec008c0e40048c0ccc0d0c0d0c0d0c0d0c0d0004cdd79805181200126103d8798000301d001302f0013021001153302902616300130200022302d302e302e001153302702216302b001323300100100c22533302a00114bd7009919299981419baf300b302100200513302d00233004004001133004004001302e002302c0013029001301b3001301b00223028302900130260013018001153302001d16323300100100922533302400114c103d87a80001323253330223375e600a603600401c266e952000330270024bd7009980200200098140011813000918120009bab3022001302200130210023758603e002603e002603c0046eb0c070004c038014c068004c03000454cc05004458c060004c060008c058004c02000c526153301049011856616c696461746f722072657475726e65642066616c736500136563001004232533300f3370e90000008991919191919191919191919299980f981100109924c64a66603866e1d200000115333020301500a149854cc07406c5854ccc070cdc3a40040022a666040602a0142930a9980e80d8b0a9980e80b0b180c0048a9980e00b8b1bae30200013020002375c603c002603c0046eb8c070004c070008dd7180d000980d001180c000980c0011bad30160013008002153301000916300b001375c002460166ea800524128436f6e73747220696e646578206469646e2774206d61746368206120747970652076617269616e74004901254578706563746564206f6e20696e636f727265637420426f6f6c65616e2076617269616e74004901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564002300637540029201244578706563746564206f6e20696e636f727265637420436f6e7374722076617269616e740049011d4578706563746564206e6f206669656c647320666f7220436f6e737472005734ae7155ceaab9e5573eae815d0aba21',
        [authToken],
        { dataType: 'list', items: [{ dataType: 'bytes' }] } as any,
      ),
    }
  },
  {
    oldState: {
      title: 'TreeState',
      anyOf: [
        {
          title: 'TreeState',
          dataType: 'constructor',
          index: 0,
          fields: [
            { dataType: 'integer', title: 'operationCount' },
            {
              title: 'operation',
              anyOf: [
                {
                  title: 'AdatagAdded',
                  dataType: 'constructor',
                  index: 0,
                  fields: [],
                },
                {
                  title: 'AdatagRemoved',
                  dataType: 'constructor',
                  index: 1,
                  fields: [],
                },
              ],
            },
            { dataType: 'bytes', title: 'adatag' },
            { dataType: 'bytes', title: 'size' },
            { dataType: 'bytes', title: 'rootHash' },
            { dataType: 'bytes', title: 'mintingPolicy' },
          ],
        },
      ],
    },
  },
  { _r: { title: 'Data', description: 'Any Plutus data.' } },
) as unknown as StateHolderStateHolder

export interface TimeDepositTimedeposit {
  new (params: { collector: string; collectionTime: bigint }): Validator
  datum: Data
  rdmr: 'Collect' | 'Redeem'
}

export const TimeDepositTimedeposit = Object.assign(
  function (params: { collector: string; collectionTime: bigint }) {
    return {
      type: 'PlutusV2',
      script: applyParamsToScript(
        '590740010000323232323232323232323232323232222232533300c3232323253330103370e9000002099999800a51008375c600460140126eb4c00cc0280240184c94ccc044cdc3a4000601e002264646464a666032603800426666600c94003400c00402c54cc05804458dd6980d000980d0011bae3018001300b001153301200e1632323232323253301737326600201c910100100e3001001222533333301f00213232323233009533301b337100069007099b80483c80400c54ccc06ccdc4001a410004266e04cdc0241002800690070b19b8a489012800001533301e001133714911035b5d2900004133714911035b5f2000375c603a66600e00266ec1300102415d00375266e292210129000042233760980103422c2000375266601001000466e28dd7180f0009bae301f001375860380046eb4c068004c8cdd81ba8301a001374e60360026ea80084c94ccc0700044cdc5245027b7d00002133714911037b5f2000375c603664646600200200644a66603e00220062664466ec130103422c20003752666012012603e00466e29221023a20003330090093020002337146eb8c07c004dd71810000981080099801001181100099bb04c10342207d0037520046eac0084c94ccc0700044cdc52441025b5d00002133714911035b5f2000375c603666600a00266ec1300102415d0037520044466ec1300103422c2000375266600c00c00466e28dd7180e0009bae301d001375800426600a6eb40080044c8cdc5244102682700332232333001001003002222533301d3371000490000800899191919980300319b8100548008cdc599b80002533302033710004900a0a40c02903719b8b33700002a66604066e2000520141481805206e0043370c004901019b8300148080cdc700300119b81371a002900119b8a4881012700002375c004444646600200200844a6660380022008266006603c00266004004603e00244646600200200644a66602c66e1c0052000133714910101300000315333016337100029000099b8a489012d003300200233702900000089980299b8400148050cdc599b803370a002900a240c00066002002444a66602666e2400920001001133300300333708004900a19b8b3370066e14009201448180004888894cc055240104416c6d6100132325333016533301600214a22a6602e9210d76616c6964203f2046616c73650014a02a66602c002294454cc05d2401117369676e65645f6279203f2046616c73650014a029414ccc054c8c8cc004004008894ccc07000452809919299980d19b8f00200914a226600800800260400046eb8c078004dd6180d980e180e180e180e180e180e180e180e1807980398078010a5115330164913d6c6973742e68617328636f6e746578742e7472616e73616374696f6e2e65787472615f7369676e61746f726965732c206372656429203f2046616c73650014a0a66602866ebd300103d87980000041533301400514a22a6602a9210f69735f636f6c6c203f2046616c73650014a0264a66602a646464a66603066e1d200230160011323232533301b002100114a06464a66603866e1d200000114a02a66603866e1d200200113232533301e3370e900118101baa301130183011301800813371200c002266e20018004dd69811800980b0010a513019001300c3014300d30140043232533301b3370e90000008a511533301b3370e900100089919299980e99b8748008c07cdd51808180b9807980b803899b8900100513371000200a6eb4c088004c054008528180c00098059809980598098019bad301e301200115330190151630093011300930110023374a90001980d99ba548000cc06ccdd2a4004660366ea00112f5c06603666602c94530103d87a80004c0103d87980004bd701980da6010ad8799fd87b80d87a80ff004bd70180d980e180e180e180e180e180e180e1807980398078010a5115330164913e74696d655f656c617073656428636f6e746578742e7472616e73616374696f6e2e76616c69646974795f72616e67652c2074696d6529203f2046616c73650014a064a66602a66e1d2000001132323232533301d3020002149854cc06805458dd6980f000980f0011bae301c001300f005153301601016301200423015001230143015001300a003149854cc03524011856616c696461746f722072657475726e65642066616c7365001365632533300c3370e90000008a99980818030018a4c2a6601a0162c2a66601866e1d2002001153330103006003149854cc03402c5854cc03401c58c0240088c028dd5000a48128436f6e73747220696e646578206469646e2774206d61746368206120747970652076617269616e74004901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e206578706563746564004901244578706563746564206f6e20696e636f727265637420436f6e7374722076617269616e740023005375400292011d4578706563746564206e6f206669656c647320666f7220436f6e737472005734ae7155ceaab9e5573eae815d0aba257481',
        [params],
        {
          dataType: 'list',
          items: [
            {
              title: 'TimeDepositParams',
              anyOf: [
                {
                  title: 'TimeDepositParams',
                  dataType: 'constructor',
                  index: 0,
                  fields: [
                    { dataType: 'bytes', title: 'collector' },
                    { dataType: 'integer', title: 'collectionTime' },
                  ],
                },
              ],
            },
          ],
        } as any,
      ),
    }
  },
  { datum: { title: 'Data', description: 'Any Plutus data.' } },
  {
    rdmr: {
      title: 'TimeDepositRedeemer',
      anyOf: [
        { title: 'Collect', dataType: 'constructor', index: 0, fields: [] },
        { title: 'Redeem', dataType: 'constructor', index: 1, fields: [] },
      ],
    },
  },
) as unknown as TimeDepositTimedeposit