import React from 'react'

export const HowItWorks: React.FC = () => (
  <>
    <section
      key="how"
      id="how"
      className=" bg-[#f5f4ee] py-8 rounded-[72px] px-8-"
    >
      <div className="container xl:max-w-4xl 2xl:max-w-5xl mx-auto mt-40 mb-40">
        <div>
          <h2 className="text-6xl font-semibold mb-4 tracking-tighter">
            How minting and claiming <br />
            processes work
          </h2>
          <p className="text-2xl mt-10 font-medium tracking-tighter mb-24">
            Managing your unique usernames <br />
            as NFTs on the Cardano blockchain <br /> are simple processes
          </p>
        </div>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-20 ">
          <div className="p-0">
            <h3 className="text-2xl font-semibold mb-2">Minting</h3>
            <ol className="list-decimal tracking-tighter text-lg mt-4 font-medium list-inside mb-6">
              <p className="text-xl mt-4">Connect your light wallet.</p>
              <p className="text-xl mt-4">
                Navigate to the <strong>Mint</strong> tab on the website.
              </p>
              <p className="text-xl mt-4">Enter the adatag you want to mint.</p>
              <p className="text-xl mt-4">
                If available then click the "Mint adatag" button.
              </p>
              <p className="text-xl mt-4">
                The website will construct a minting transaction for your adatag
                NFT based on your input.
              </p>
              <p className="text-xl mt-4">
                Sign and submit the transaction using your connected wallet.
              </p>
              <p className="text-xl mt-4">
                Once the transaction is confirmed, your adatag NFT will be
                minted.
              </p>
            </ol>
            <p className="mb-4 tracking-tighter opacity-75">
              Please note that during periods of high chain load, it can take
              several hours for the minting process to complete.
            </p>
          </div>
          {/*<div className="p-0">
              <h3 className="text-2xl font-semibold mb-2">Burning</h3>
              <ol className="list-decimal tracking-tighter text-lg mt-4 font-medium list-inside mb-6">
                <p className="text-xl mt-4">Connect your light wallet.</p>
                <p className="text-xl mt-4">Navigate to the <strong>Burn</strong> tab on the website.</p>
                <p className="text-xl mt-4">Search for an available adatag to mint</p>
                <p className="text-xl mt-4">Click the "Mint adatag" button</p>
                <p className="text-xl mt-4">The website will construct a minting transaction for your adatag NFT based on your input.</p>
                <p className="text-xl mt-4">Sign and submit the transaction using your connected wallet.</p>
                <p className="text-xl mt-4">Once the transaction is confirmed, your adatag NFT will be minted to your wallet address.</p>
              </ol>
              <p className="mb-4 tracking-tighter opacity-75">Please note that burning an adatag is a permanent action and cannot be undone.</p>
            </div>*/}
          <div className="p-0">
            <h3 className="text-2xl font-semibold mb-2">Claiming</h3>
            <ol className="list-decimal tracking-tighter text-lg mt-4 font-medium list-inside mb-6">
              <p className="text-xl mt-4">Connect your light wallet.</p>
              <p className="text-xl mt-4">
                Navigate to the <strong>Claim</strong> tab on the website.
              </p>
              <p className="text-xl mt-4">
                Enter the adatag for which you want to claim the deposit.
              </p>
              <p className="text-xl mt-4">Click the "Claim deposit" button.</p>
              <p className="text-xl mt-4">
                The website will construct a claim transaction for the deposit
                based on your input.
              </p>
              <p className="text-xl mt-4">
                Sign and submit the transaction using your connected wallet.
              </p>
              <p className="text-xl mt-4">
                Once the transaction is confirmed, the deposit will be released
                to your wallet address.
              </p>
            </ol>
            <p className="mb-4 tracking-tighter opacity-75">
              Please note that you can only claim a deposit after the 20-day of
              minting and if you are the original minter of the adatag.
            </p>
          </div>
        </div>
      </div>
    </section>
  </>
)
