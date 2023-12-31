import * as P from "../config/plutus.ts";
import { AppSettings } from "../config/config.ts";
import { Script, Credential, SpendingValidator, Data, Assets } from "translucent-cardano";
import sha256 from "sha256";

export type MintingDetails = {
    mintingPolicy: Script;
    mintingPolicyId: string;
    stateholderValidator: Script;
    params: MintingParams;
};

export type MintingParams = {
    authToken: string;
    stateHolder: string;
    timedepositValidator: string;
    deactivationTime: bigint;
    depositBase: bigint;
    lockingDays: bigint;
    adahandle: string;
};

export type CollectionDetails = {
    timelockScript: Script;
    timeLockAddress: string;
    params: CollectionParams;
};

type CollectionParams = {
    collector: string;
    collectionTime: bigint;
};

export type UtxoReference = {
    utxoRef: {
        transactionId: { hash: string };
        outputIndex: bigint;
    };
};

const emptyBlake2b224 = [
    0x83, 0x6c, 0xc6, 0x89, 0x31, 0xc2, 0xe4, 0xe3, 0xe8, 0x38, 0x60, 0x2e, 0xca,
    0x19, 0x02, 0x59, 0x1d, 0x21, 0x68, 0x37, 0xba, 0xfd, 0xdf, 0xe6, 0xf0, 0xc8,
    0xcb, 0x07,
];

const emptySha2256 = [
    0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14, 0x9a, 0xfb, 0xf4, 0xc8, 0x99,
    0x6f, 0xb9, 0x24, 0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c, 0xa4, 0x95,
    0x99, 0x1b, 0x78, 0x52, 0xb8, 0x55,
];

export class Adatag {
    private appSettings: AppSettings;

    public referenceAddress: string;
    public authMintingPolicy: Script;
    public mintingDetails: MintingDetails;
    public collectionDetails: CollectionDetails;

    private constructor(ownerOutRef: UtxoReference, collectorCred: Credential, appSettings: AppSettings) {
        this.appSettings = appSettings;
        this.referenceAddress = this.getReferenceAddress();
        const { authMintingPolicy, policyId } = this.getAuthTokenScript(ownerOutRef);
        this.authMintingPolicy = authMintingPolicy;
        this.collectionDetails = this.getTimelockDetails(collectorCred.hash);
        this.mintingDetails = this.getMintingDetails(policyId, this.collectionDetails.timelockScript);
    }

    private static instance: Adatag;

    public static async getInstance(ownerOutRef: UtxoReference, collectorCred: Credential, appSettings: AppSettings) {
        if (!this.instance) {
            this.instance = new Adatag(ownerOutRef, collectorCred, appSettings);
        }
        return this.instance;
    }

    private getReferenceAddress() {
        const referenceScript = new P.AlwaysFailAlwaysFail();
        const alwaysFailHash = this.appSettings.translucent.utils.validatorToScriptHash(referenceScript);
        const referenceAddress = this.appSettings.translucent.utils.credentialToAddress(
            this.appSettings.translucent.utils.scriptHashToCredential(alwaysFailHash)
        );
        return referenceAddress;
    }

    private getAuthTokenScript(outRef: UtxoReference) {
        const authMintingPolicy = new P.OneshotAuthToken({
            transactionId: { hash: outRef.utxoRef.transactionId.hash },
            outputIndex: BigInt(outRef.utxoRef.outputIndex),
        });
        const policyId = this.appSettings.translucent.utils.mintingPolicyToId(authMintingPolicy);
        return { authMintingPolicy, policyId };
    }

    private getTimelockDetails(collectorPkh: string): CollectionDetails {
        const collectionTime = BigInt(this.appSettings.appParams.collectionTime);
        const timeLock = new P.TimeDepositTimedeposit({
            collector: collectorPkh,
            collectionTime: collectionTime,
        });
        const timeLockHash = this.appSettings.translucent.utils.validatorToScriptHash(timeLock);
        const timelockAddress = this.appSettings.translucent.utils.credentialToAddress(
            this.appSettings.translucent.utils.scriptHashToCredential(timeLockHash),
        );
        return {
            timelockScript: timeLock,
            timeLockAddress: timelockAddress,
            params: {
                collector: collectorPkh,
                collectionTime: collectionTime,
            },
        };
    }

    private getMintingDetails(authPolicyId: string, timelock: SpendingValidator): MintingDetails {
        const stateholderValidator = new P.StateHolderStateHolder(authPolicyId);
        const stateholderHash = this.appSettings.translucent.utils.validatorToScriptHash(stateholderValidator);
        const stateholderCredential =
            this.appSettings.translucent.utils.scriptHashToCredential(stateholderHash);
        const stateholderAddress = this.appSettings.translucent.utils.credentialToAddress(
            stateholderCredential,
        );
        const timelockHash = this.appSettings.translucent.utils.validatorToScriptHash(timelock);
        const params = {
            authToken: authPolicyId,
            stateHolder: stateholderHash.toString(),
            timedepositValidator: timelockHash.toString(),
            deactivationTime: BigInt(this.appSettings.appParams.deactivationTime),
            depositBase: BigInt(this.appSettings.appParams.depositBase),
            lockingDays: BigInt(this.appSettings.appParams.lockingDays),
            adahandle: this.appSettings.appParams.adahandleSymbol,
        };
        const mintingPolicy = new P.AdatagAdatagMinting(params);
        const mintingPolicyId = this.appSettings.translucent.utils.mintingPolicyToId(mintingPolicy);
        return {
            mintingPolicy,
            mintingPolicyId,
            stateholderValidator,
            params,
        };
    }

    private scriptToAddress(script: Script) {
        const scriptHash = this.appSettings.translucent.utils.validatorToScriptHash(script);
        const scriptCred =
            this.appSettings.translucent.utils.scriptHashToCredential(scriptHash);
        const scriptAddress = this.appSettings.translucent.utils.credentialToAddress(
            scriptCred,
        );
        return scriptAddress;
    }

    public async deploy() {
        const tx = this.appSettings.translucent
            .newTx()
            .payToContract(
                this.referenceAddress,
                { inline: Data.void(), scriptRef: this.collectionDetails.timelockScript },
                {},
            )
            .payToContract(
                this.referenceAddress,
                { inline: Data.void(), scriptRef: this.mintingDetails.stateholderValidator },
                {},
            )
            .payToContract(
                this.referenceAddress,
                { inline: Data.void(), scriptRef: this.mintingDetails.mintingPolicy },
                {},
            );

        let assets: Assets = {};

        const sateholderAddress = this.scriptToAddress(this.mintingDetails.stateholderValidator);

        for (let i = 97; i <= 122; i++) {
            const val = [48, i - 1, i + 1];
            const hash_val = sha256(Array.from(val), { asBytes: true });

            const root = hash_val.concat(emptySha2256, emptySha2256);
            const root_hash = sha256(Array.from(root));

            const state: P.StateHolderStateHolder["oldState"] = {
                operationCount: 0n,
                operation: "AdatagRemoved",
                adatag: "",
                size: "0",
                rootHash: root_hash,
                mintingPolicy: this.mintingDetails.mintingPolicyId,
            };

            const id = Data.to(state, P.StateHolderStateHolder.oldState);

            const assetId = this.mintingDetails.params.authToken + i.toString(16);
            assets[assetId] = 1n;
            tx.payToContract(sateholderAddress, { inline: id }, { [assetId]: 1n });
        }

        const txCompleted = await tx
            .attachMintingPolicy(this.authMintingPolicy)
            .mintAssets(assets, Data.void())
            .complete();

        const signedTx = await txCompleted.sign().complete();

        //console.log(`\n\n Transaction: \n ${signedTx.txSigned.to_json()}`);
        const txHash = await signedTx.submit();
        // console.log(`TxHash ${txHash}`);

        return txHash;
    }
}