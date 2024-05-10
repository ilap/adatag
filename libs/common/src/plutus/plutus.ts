/* eslint-disable @typescript-eslint/no-explicit-any */
import { applyParamsToScript, Data, Validator } from 'translucent-cardano'
import { MintRedeemer } from './types'

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
  rdmr: MintRedeemer
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
        '590d40010000323232323232322232323225333007323232533300a3007300b37540022646464646464646464646464a666032603800426464a66466032600260346ea80084c8c8c8c8c8c94ccc088c0940084c94ccc080c020c084dd50008991919191919299919813999191192999815a9998159802800899b890014808052809919299981698031801240002a66605a600a00226464666002002945200022253330313370e0020082004266600600664a666064a6660646014002294454ccc0c8cdc3800a417c02294454ccc0c8cdc3800a40b429444cdc3800a40b820062940c018004cdc0000a40046e340105280a5030013370000490009b8e00214a0600a0024a6660526004002294454ccc0a4cdc4a40c0002266e24005207214a04a66605066e2520c201001133712002907a008a50375c602260526ea801c54ccc09cc090c0a0dd50128992999814181200209919299981519299981599192999816981498171baa00113232533302f32325333031302e00114a22a666062605a002264a66606464a66606c606a0022a666066605e6068002294454ccc0ccc0c0c0d00045280b0b1baa302030343754604260686ea801c4cdc4800802899b88001005375a606c60666ea800852818189baa001301e30313754603c60626ea801040045281919299981818168008a5015333030302c0011325333031325333035303400115333032302e303300114a22a666064605e606600229405858dd5180f98199baa301f3033375400c266e240100044cdc40020009bad3035303237540042944c0c0dd5000980e98181baa301c303037540066eb4c0c8c0bcdd50008b180d98171baa301b302e375403860126605e60126605e66e9520023302f3003302d375405497ae03302f33302b4a2980103d87a80004c0103d87980004bd7019817a6010ad8799fd87b80d87a80ff004bd700a511533302b32533302c3028302d375464a666060002298103d87a80001300b3303133302d3007302e3754606400298103d87a80004c0103d87980004bd7019198008008101129998188008a5eb804c8c94ccc0c0c0b0c0c4dd500089980200200109981a199818180518189baa303530323754002980103d87a80004c0103d879800033004004002533302f300933300c3756603a60626ea8c0d00080100145300107d8799fd87a80ff0014c103d87a8000303400114a02944dd7181818189818981898189818981898169baa02a14a226464a6660606066004264a66605c602c605e6ea80044c8c8c94ccc0c4ccc8894ccc0d0c0c00044cdc48018010a9919981a980e801099b8930014801000c54ccc0d4cdc3a400c004266e24c0052008003153330353370e9004001099b8930014804000c54ccc0d4cdc3a4014004266e24c0052020003153330353370e9006001099b8930014810000c4cdc4a403c0066e0c00cdd6980418199baa03033300e3756603e60666ea8015220100488100300c0071325333032302e30333754002266e24cdc01bad3037303437540026eb4c0dcc0e0c0e0c0e0c0e0c0e0c0d0dd50188010b181018199baa301f303337540422940dd6980f18191baa002533302f302c30303754002264646464a66606c60720042930b1bad30370013037002375c606a00260626ea800458c0ccc0c0dd50008b180b98179baa0011630310013300f01e375c602a605a6ea80a8dd7180a18161baa00a1333333332323232322222222323232533303933333300b4a0010008006004002266666601694401c01000c008004528299981c299981c19baf3300b375c604c60746ea8014025300103d879800013375e660160126eb8c088c0e8dd5002a60103d879800014a02646607a0026607aa66607266ebc01801440044014cc0f4c05ccc0f4dd48049981e9ba900a3303d3023303b375400c97ae04bd70180b1981e1813981d1baa0053303c3026303a375400a660786ea40252f5c02c6018006601600644444464646464a666070602460726ea8c0f401054ccc0e00084cdc78008048a5014a06eb8c0f0c0f4008c040c0dcdd5181d800981d800991999800800a504a000a4444a666070606860726ea80044cc0f0ccc0e000d30103d87a80004c0103d87980003303c3330380024c0103d87a80004c0103d87980003303c303d303a375400297ae0132323232323232323232323232323232325333049533304901b1533304900e14a2201a29404c8c8c8c8c8c8cc14cccc13d4ccc13c03c528880526103d87a80004c0103d87980003305333304f533304f00d14a2201098103d87a80004c0103d87980003305337526660466eb8c150008dd7182a0009bae305430550014bd70182a000a999826a99982699baf00400214a2202226464660a66ea4c090010cc14cc140004cc14cc1440052f5c0a6466609ea66609e60020182600200e29404cdd81ba9002375200e2a66609ea66609e66609e600201894128898008038a501337606ea4030dd48010b1b8f488120e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b85500333021302200148920e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b85500488120e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855001533304d01213305137526044008660a26ea4028cc144dd4802a5eb8058c144c148008c140004c14005cc1380584cc134ccc1254ccc1240245288802260103d87a80004c0103d87980003304d333049533304900714a2200498103d87a80004c0103d87980003304d375266603a02000c00297ae0375c609a609c004604260906ea8c130004c130008c07cc118dd518250009999808008003803182498250051bae30483049002301c30433754608e002608e004603460826ea8c114004cccc02c02c008004c1100154ccc0f80205288801299981e8040a5110023371e00601466e3c008028c100008dd7181f000981d1baa00122533302f337200040022980103d87980001533302f3371e0040022980103d87a800014c103d87b800022230053371400666e280080048c008cdc51bae301b302e375400266e28dd7180d18171baa001375c602c605c6ea80048dc90009bae3014302c37540146eb8c008c0b0dd50051bae3001302c37540226eb8c004c0b0dd5005180c98161baa0033018302c3754006602860586ea800c528118179818181818181818000918171817981798178008a50302c3029375404a2a66604e66e1c00d200114a02940dc42400029408dc6800999800991980080080b1129998148008a5eb7bdb1804c8c8c8c94ccc0a8cdc7a44100002100313302e337606ea4008dd3000998030030019bab302b003375c6052004605a00460560020346eb8c038c098dd5002111192999813981198141baa0011480004dd6981618149baa0013253330273023302837540022980103d87a8000132330010013756605a60546ea8008894ccc0b0004530103d87a80001323232533302c3371e00e6eb8c0b400c4c028cc0c0dd4000a5eb804cc014014008dd698168011818001181700099198008008021129998158008a6103d87a80001323232533302b3371e00e6eb8c0b000c4c024cc0bcdd3000a5eb804cc014014008dd59816001181780118168009ba548000c01c004c094c088dd50008b180498109baa00116302300133001010375c6016603e6ea807088cc02c0088c94ccc080c070c084dd5000899b8f003375c604a60446ea8004528180718109baa300e3021375400260020044a666036603060386ea80044c8c8c8c8c8c8c8c8c8c8c8c94ccc0a8c0b40084c92632533302830250011533302b302a37540142930b0a99981418120008a99981598151baa00a14985858c0a0dd50048b1bae302b001302b002375c605200260520046eb8c09c004c09c008dd718128009812801181180098118011bad3021001301d37540022c603c60366ea8008dc3a40082c600260326ea8c014c064dd50011180e180e980e8008b180d000999119801801119299980c180a180c9baa00113371e0066eb8c074c068dd50008a50300630193754600c60326ea8c014c064dd50009bac3019009375c6004602c6ea804c88c8cc00400400c894ccc06800452f5c026464a666032600a00426603a00466008008002266008008002603c00460380024603060320024602e002602a602c602c602c0046eacc050004c050c050008dd618090009809180900098069baa003375c601e60186ea800458c038c03c008c034004c024dd50008a4c26cac64646464a666012600c00226464a66601c6022004264932999805980418061baa0011323232323232533301430170021323232498c03400cc02c010c02801458c054004c054008c04c004c04c008c044004c034dd50008b0b180780098059baa0071533300930050011533300c300b375400e2930b0b18049baa006253330083005300937540022646464646464a66602260280042930b1bae30120013012002375c602000260200046eb8c038004c028dd50008b180080091192999804180280089919191919192999808980a0010991924c66014014004660120120062c60240026024004602000260200046eb8c038004c028dd50010a999804180200089919299980698080010a4c2c6eb8c038004c028dd50010b18041baa001370e90011b87480015cd2ab9d5573caae7d5d02ba15745',
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
        } as any
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
                            { title: 'left', $ref: '#/definitions/ilap~1integri_tree~1types~1Proof' },
                            { title: 'right', $ref: '#/definitions/ilap~1integri_tree~1types~1Proof' },
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
  }
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
        '585401000032323232232232253330064a029309b2b19299980299b8748000c01800454ccc020c01c0045261616375400264a66600666e1d2000300400115333006300500114985858dd5000ab9a5573aaae7955cf81',
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
  }
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
        '583701000032323232232253330044a229309b2b19299980199b8748000c01000454ccc018c01400452616163754002ae6955ceaab9e5573e1',
    }
  },

  {
    _r: {
      title: 'Unit',
      description: 'The nullary constructor.',
      anyOf: [{ dataType: 'constructor', index: 0, fields: [] }],
    },
  }
) as unknown as AlwaysMintMint

export interface OneshotAuthToken {
  new (utxoRef: { transactionId: { hash: string }; outputIndex: bigint }): Validator
  _r: undefined
}

export const OneshotAuthToken = Object.assign(
  function (utxoRef: { transactionId: { hash: string }; outputIndex: bigint }) {
    return {
      type: 'PlutusV2',
      script: applyParamsToScript(
        '58b0010000323232323232322232322533300632325333008300530093754601a601c0042a666010600a60126ea8c8cc004004008894ccc0340045300103d87a800013232533300c3375e6022601c6ea800802c4cdd2a40006602000497ae01330040040013011002300f00114a2294058dd6180618049baa300c0013008375400229309b2b192999802980118030008a99980418038008a4c2c2c6ea8008dc3a4000ae6955ceaab9e5573eae815d0aba201',
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
        } as any
      ),
    }
  },

  {
    _r: {
      title: 'Unit',
      description: 'The nullary constructor.',
      anyOf: [{ dataType: 'constructor', index: 0, fields: [] }],
    },
  }
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
        '5904560100003232323232323223223232323222533300a323232533300d3008300e37540022646464646464a666026601e60286ea80044c8c8c8c94ccc068c0740084c8c94ccc064cdc3a400860346ea80044c8c8c8c8c94ccc078cccc004c8cc00400403c894ccc08c00452f5bded8c0264646464a66604866e3d221000021003133028337606ea4008dd3000998030030019bab3025003375c6046004604e004604a0026eb8c008c080dd50029bae30073020375400aa66603c006290010a4002264a66603e666600400203a666e31200048008dd7180418109baa0064800854cc8cc080cdc399918008009129998128008a400026006660040046050002646600200200644a66604a002297ae013302630233027001330020023028001480104c8c8c8c94ccc090cdc39bad301330263754016600a6eb4c04cc098dd50108a99981219b894800000454ccc090cdc380099b8000253330240091480085200113371e6eb8c020c098dd50109bae30083026375401629405280a503002375c6006604a6ea8028c004dd7180118121baa01f23253330233370e66e3800920004818054ccc08cc07800452000161323233300100148001200022253330273370e0020082004266600600664a666050a66605066e25206000113371200290390a5013370066e0800d201433700002902f8b19b8e0060013008001371a0046e340048c098c09cc09cc09c004dc0240042c2c6eacc02cc080dd50040b1111191929998131814801099299981219b8f375c604a00400a266e1c0040105281bad3025001163027001325333022301d30233754002297adef6c6013756604e60486ea8004c8cc004004014894ccc0980045300103d87a8000132323253330263371e0106eb8c09c00c4cdd2a4000660546e980052f5c026600a00a0046eacc09c008c0a8008c0a00048c088c08cc08cc08cc08cc08c004cdd79804180e9baa0024c0103d87980003014001301e301b37540022c600260346ea80088c074c078c07800458c06c004c8cc00400401c894ccc06800452f5c026464a66603266ebcc020c06cdd500100289980e80119802002000899802002000980f001180e000980c980b1baa3001301637546032602c6ea80088c064c06800458cc88c8cc00400400c894ccc0640045300103d87a80001323253330183375e600e60346ea80080144cdd2a40006603800497ae0133004004001301d002301b0013758602e00a602e60286ea80188c05c004dd5980a980b180b0011bac301400130143014001300f37540042c60226024004602000260186ea8004526136563001004253330073003300837540022646464646464646464646464a66602c60320042649319299980a18080008a99980b980b1baa00a14985854ccc050c03c00454ccc05cc058dd50050a4c2c2c60286ea802458dd7180b800980b8011bae30150013015002375c602600260260046eb8c044004c044008c03c004c03c008dd6980680098049baa00116370e90011b8748000dd7000ab9a5573aaae7955cfaba05742ae89',
        [authToken],
        { dataType: 'list', items: [{ dataType: 'bytes' }] } as any
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
                { title: 'AdatagAdded', dataType: 'constructor', index: 0, fields: [] },
                { title: 'AdatagRemoved', dataType: 'constructor', index: 1, fields: [] },
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
  { _r: { title: 'Data', description: 'Any Plutus data.' } }
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
        '5902c80100003232323232323222232323225333008323232533300b3007300c3754010266666002944024dd7180118069baa00a375a6006601a6ea802801054ccc02cc01cc030dd50048991919192999809180a801099999802a5000d00300100816375a602600260260046eb8c044004c034dd50048b11111192999808299980819baf4c0103d8798000005100613232325333013300e3014375400226464a66602a6464a66602e6026002294454ccc05cc0480044c94ccc060c94ccc070c06c00454ccc064c050c0680045288a99980c980a980d0008a5016163754602060346ea8c03cc068dd5003899b8900100513371000200a6eb4c070c064dd50010a50301737540026018602e6ea8c030c05cdd500208008a5032325333016301200114a02a66602c6022002264a66602e64a66603660340022a66603060266032002294454ccc060c050c0640045280b0b1baa300f30193754601e60326ea80184cdc4802000899b88004001375a603660306ea8008528980b1baa001300b301637546018602c6ea800cdd6980c180a9baa00116300930143754601260286ea8c05cc060c060c060c060c060c060c060c050dd51804980a1baa0043374a90001980a99ba548000cc054cdd2a40046602a6ea00112f5c06602a666022945300103d87a80004c0103d87980004bd701980aa6010ad8799fd87b80d87a80ff004bd702999808180618089baa0051323232325333017301a002149858dd6980c000980c0011bae30160013012375400a2c20022940c8cc004004dd6180a980b180b180b180b180b180b180b180b18091baa30073012375400444a66602800229404c94ccc048cdc79bae301700200614a2266006006002602e0024601e0024601c601e00229309b2b19299980398018008a99980518049baa00414985854ccc01cc00800454ccc028c024dd50020a4c2c2c600e6ea800cdc3a40046e1d20005734aae7555cf2ab9f5740ae855d101',
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
        } as any
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
  }
) as unknown as TimeDepositTimedeposit
