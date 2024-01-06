import { applyParamsToScript, Data, Validator } from 'translucent-cardano'

/*
  For using recursive type device the invariant as below:
    const mintRedeemer: P.MintRedeemer = {
      Minting: [
        {
          updateVal: rootVal1,
          appendVal: rootVal1,
          proof: proof,
        },
      ], // Use 'as const' to assert that Minting is a tuple with a single element
    }
   
    const rdmr = Data.to(mintRedeemer, P.AdatagAdatagMinting.rdmr, 'proof')
 */
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
