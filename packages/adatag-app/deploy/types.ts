export interface PlutusParams {
  hashAlg: string;
  collectorAddress: string;
  collectionTime: number;
  depositBase: number;
  deactivationTime: number;
  lockingDays: number;
  adahandle: string;
}

/**
 * Represents the deployment details necessary for running the application.
 * The application also relies on proper provider settings for accessing the blockchain.
 */
export interface BootstrapDetails {
  network: string;
  hashAlg: string;
  // Api provider can be different from the provider used for deployment.
  genesisTransaction: string;
  bootstrapTime: {
    epoch: number;
    date: string;
  };
  referenceScript: {
    scriptHash: string;
    scriptAddress: string;
  };
  authTokenScript: {
    policyId: string;
    params: {
      genesis_utxo: {
        txHash: string;
        outputIndex: number;
      };
    };
  };
  timelockScript: {
    scriptHash: string;
    scriptAddress: string;
    params: {
      collectorAddr: string;
      collectionTime: {
        epoch: number;
        date: string;
      };
    };
    refIndex: number;
  };
  stateholderScript: {
    scriptHash: string;
    scriptAddress: string;
    params: {
      authToken: string;
    };
    refIndex: number;
  };
  adatagMinting: {
    policyId: string;
    params: {
      lockingDays: {
        days: number;
        ms: number;
      };
      deactivationTime: {
        epoch: number;
        date: string;
      };
      depositBase: number;
      adahandle: string;
    };
    refIndex: number;
  };
}
