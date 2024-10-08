import { DataSignature } from '@meshsdk/core'
import { Wallet, CIP30Interface } from '@blaze-cardano/sdk'

declare type WalletInstance = {
  getNetworkId(): Promise<number>
  getUtxos(amount: string | undefined): Promise<string[] | undefined>
  getBalance(): Promise<string>
  getUsedAddresses(): Promise<string[]>
  getUnusedAddresses(): Promise<string[]>
  getChangeAddress(): Promise<string>
  getRewardAddresses(): Promise<string[]>

  signTx(tx: string, partialSign: boolean): Promise<string>
  signData(address: string, payload: string): Promise<DataSignature>

  submitTx(tx: string): Promise<string>
  signTxs(txs: string[], partialSign: boolean): Promise<string[]>

  experimental: ExperimentalFeatures
}

declare type ExperimentalFeatures = {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  [x: string]: any
  getCollateral(): Promise<string[] | undefined>
}

/**
 * Using the `Adapter` design pattern that allows two incompatible interfaces to work
 * together by creating an intermediate class that conforms to one interface and adapts it to the other interface.
 */
export class WalletInstanceWrapper implements CIP30Interface {
  private readonly walletInstance: WalletInstance

  constructor(walletInstance: WalletInstance) {
    this.walletInstance = walletInstance
  }

  async getNetworkId(): Promise<number> {
    return this.walletInstance.getNetworkId()
  }

  async getUtxos(): Promise<string[] | undefined> {
    return this.walletInstance.getUtxos(undefined)
  }

  async getBalance(): Promise<string> {
    return this.walletInstance.getBalance()
  }

  async getUsedAddresses(): Promise<string[]> {
    return this.walletInstance.getUsedAddresses()
  }

  async getUnusedAddresses(): Promise<string[]> {
    return this.walletInstance.getUnusedAddresses()
  }

  async getChangeAddress(): Promise<string> {
    return this.walletInstance.getChangeAddress()
  }

  async getRewardAddresses(): Promise<string[]> {
    return this.walletInstance.getRewardAddresses()
  }

  async signTx(tx: string, partialSign: boolean): Promise<string> {
    return this.walletInstance.signTx(tx, partialSign)
  }

  async signData(address: string, payload: string): Promise<{ signature: string; key: string }> {
    const dataSignature = await this.walletInstance.signData(address, payload)
    // TODO: Check in blaze what is the key.
    return { signature: dataSignature.signature, key: address }
  }

  async submitTx(tx: string): Promise<string> {
    return this.walletInstance.submitTx(tx)
  }

  async getCollateral(): Promise<string[]> {
    return (await this.walletInstance.experimental.getCollateral()) || []
  }
}
