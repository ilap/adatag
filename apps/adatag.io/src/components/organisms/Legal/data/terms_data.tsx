import React from 'react'

export const description = {
  title: 'Terms & Conditions',
  subtitle: 'Last updated: April 20, 2024',
}

export const data = [
  {
    title: 'Introduction',
    content: (
      <>
        <p>Please read these terms and conditions carefully before using Our Service.</p>
      </>
    ),
  },
  {
    title: 'Interpretation and Definitions',
    content: (
      <>
        <h3>Interpretation</h3>
        <p>
          The words of which the initial letter is capitalized have meanings defined under the following conditions. The
          following definitions shall have the same meaning regardless of whether they appear in singular or in plural.
        </p>
        <h3>Definitions</h3>
        <ul className="list-disc">
          <li>
            <strong>Affiliate</strong>: means an entity that controls, is controlled by or is under common control with
            a party, where "control" means ownership of 50% or more of the shares, equity interest or other securities
            entitled to vote for election of directors or other managing authority.
          </li>
          <li>
            <strong>Country</strong>: refers to: Hungary
          </li>
          <li>
            <strong>Service Provider</strong>: (referred to as either "I", "Me", "My" or "Our" in this Agreement) refers
            to the individual providing the Service.
          </li>
          <li>
            <strong>Device</strong>: means any device that can access the Service such as a computer, a cellphone or a
            digital tablet.
          </li>
          <li>
            <strong>Service</strong>: refers to the Website and the functionality it offers, including the construction
            of transactions for minting a username NFT (known as "@adatag"), and the facilitation of user connection to
            a light wallet for submitting the transaction onto the Cardano blockchain.
          </li>
          <li>
            <strong>Terms and Conditions</strong>: (also referred as "Terms") mean these Terms and Conditions that form
            the entire agreement between You and the Service Provider regarding the use of the Service.
          </li>
          <li>
            <strong>Third-party Social Media Service</strong>: means any services or content (including data,
            information, products or services) provided by a third-party that may be displayed, included or made
            available by the Service.
          </li>
          <li>
            <strong>Website</strong>: refers to Adatag, accessible from{' '}
            <a href="https://adatag.io">https://adatag.io</a>
          </li>
          <li>
            <strong>You</strong>: means the individual accessing or using the Service, or the company, or other legal
            entity on behalf of which such individual is accessing or using the Service, as applicable.
          </li>
        </ul>
      </>
    ),
  },
  {
    title: 'Acknowledgment',
    content: (
      <>
        <p>
          These are the Terms and Conditions governing the use of this Service and the agreement that operates between
          You and the Service Provider. These Terms and Conditions set out the rights and obligations of all users
          regarding the use of the Service.
        </p>
        <p>
          Your access to and use of the Service is conditioned on Your acceptance of and compliance with these Terms and
          Conditions. These Terms and Conditions apply to all visitors, users and others who access or use the Service.
        </p>
        <p>
          By accessing or using the Service You agree to be bound by these Terms and Conditions. If You disagree with
          any part of these Terms and Conditions then You may not access the Service.
        </p>
        <p>
          You represent that you are over the age of 18. The Service Provider does not permit those under 18 to use the
          Service.
        </p>
        <p>
          Your access to and use of the Service is also conditioned on Your acceptance of and compliance with the
          Privacy Policy of the Service Provider. Our Privacy Policy describes Our policies and procedures on the
          collection, use and disclosure of Your personal information when You use the Application or the Website and
          tells You about Your privacy rights and how the law protects You. Please read Our Privacy Policy carefully
          before using Our Service.
        </p>
      </>
    ),
  },
  {
    title: 'Reclaim/Redeem Policy',
    content: (
      <ul>
        <li>
          <strong>No Refunds:</strong> As minting is entirely free (with only transaction fees incurred), there are no
          refunds provided.
        </li>
        <li>
          <strong>Time Lock Deposit:</strong> The time lock deposit associated with minting adatags is claimable by two
          entities: the beneficiary and the collector.
        </li>
        <ul>
          <li>
            <strong>Beneficiary:</strong> The beneficiary refers to the individual or entity whose wallet address minted
            the adatag. This party has the right to redeem the deposit after a period of 20 days from the minting date.
          </li>
          <li>
            <strong>Collector:</strong> The collector is an entity designated to collect unclaimed deposits or
            donations. This entity is typically specified in the Plutus script's validator parameters. The collector may
            redeem unclaimed deposits after a certain period, typically 6 months from the minting date.
          </li>
        </ul>
        <li>
          <strong>Donation Definition:</strong> Users have the option to contribute funds to the service provider to
          support the costs of maintaining and enhancing the platform. These contributions are considered donations and
          are not refundable. Any deposit left unclaimed by the beneficiary after a period of 6 months from the minting
          date will be automatically treated as a donation. These donations will be used to cover service costs,
          maintenance, and future development of Adatag.io, and can be redeemed by the collector address specified in
          the Plutus script. It is the <strong>responsibility of the users</strong> to manage their keys and redeem
          their deposits within the specified time frame to avoid their deposits being treated as donations.
        </li>
        <li>
          <strong>Unclaimed Deposits:</strong> Users must acknowledge and accept that any deposit left unclaimed after 6
          months from the minting date will be treated as a donation. These donations may be redeemed by the collector
          address specified in the Plutus script.
        </li>
        <li>
          <strong>Developer Responsibility:</strong> It is important to note that the developer of Adatag.io is not
          responsible for any loss of access to beneficiary keys required for redeeming deposits.
        </li>
      </ul>
    ),
  },
  {
    title: 'Minting @adatag NFTs',
    content: (
      <>
        <p>
          The Service allows You to construct a minting transaction for minting a username called "@adatag" NFT based on
          Your input. You can sign and submit this transaction onto the Cardano blockchain using a browser extension's
          light wallet connected to the Adatag's website.
        </p>
        <p>
          When You connect a wallet, You must agree to these Terms and Conditions, the Privacy Policy, and the
          Disclaimer. The Plutus smart contract for minting is an open-source script developed by the Service Provider.
        </p>
      </>
    ),
  },
  {
    title: '@adatag Features',
    content: (
      <>
        <ul className="list-disc">
          <li>
            <strong>Decentralized</strong>: @adatag is not controlled by any single entity, making it more secure and
            resistant to censorship.
          </li>
          <li>
            <strong>Free</strong>: @adatag username creation and deletion is completely free, with no price or royalty
            fees.
          </li>
          <li>
            <strong>Instant</strong>: @adatag usernames can be created and deleted instantly, without any third-party
            involvement.
          </li>
          <li>
            <strong>Public</strong>: @adatag usernames are publicly stored on-chain, making them accessible to everyone.
          </li>
          <li>
            <strong>Transparent</strong>: All code is open-source. The generated policy IDs can be independently
            checked.
          </li>
        </ul>
      </>
    ),
  },
  {
    title: 'Time-Lock Deposit',
    content: (
      <>
        <p>
          Initially, users must time-lock deposit a certain value to prevent abuse of the system. These deposits are
          redeemable by the rightful beneficiaries after a certain time (preferably 20 days) of minting.
        </p>
      </>
    ),
  },
  {
    title: 'Links to Other Websites',
    content: (
      <>
        <p>
          Our Service may contain links to third-party web sites or services that are not owned or controlled by the
          Service Provider.
        </p>
        <p>
          The Service Provider has no control over, and assumes no responsibility for, the content, privacy policies, or
          practices of any third party web sites or services. You further acknowledge and agree that the Service
          Provider shall not be responsible or liable, directly or indirectly, for any damage or loss caused or alleged
          to be caused by or in connection with the use of or reliance on any such content, goods or services available
          on or through any such web sites or services.
        </p>
        <p>
          We strongly advise You to read the terms and conditions and privacy policies of any third-party web sites or
          services that You visit.
        </p>
      </>
    ),
  },
  {
    title: 'Termination',
    content: (
      <>
        <p>
          We may terminate or suspend Your access immediately, without prior notice or liability, for any reason
          whatsoever, including without limitation if You breach these Terms and Conditions.
        </p>
        <p>Upon termination, Your right to use the Service will cease immediately.</p>
      </>
    ),
  },
  {
    title: 'Limitation of Liability',
    content: (
      <>
        <p>
          As our service is an open-source project and does not involve the sale of any products or services, the
          Service Provider and its suppliers have no liability whatsoever for any damages that you may incur through the
          use of or inability to use the Service, third-party software and/or third-party hardware used with the
          Service, or otherwise in connection with any provision of these Terms.
        </p>

        <p>
          To the maximum extent permitted by applicable law, in no event shall the Service Provider or its suppliers be
          liable for any special, incidental, indirect, or consequential damages whatsoever (including, but not limited
          to, damages for loss of profits, loss of data or other information, for business interruption, for personal
          injury, loss of privacy arising out of or in any way related to the use of or inability to use the Service,
          third-party software and/or third-party hardware used with the Service, or otherwise in connection with any
          provision of these Terms), even if the Service Provider or any supplier has been advised of the possibility of
          such damages and even if the remedy fails of its essential purpose.
        </p>

        <p>
          You acknowledge and agree that the Service Provider is not responsible for any losses, damages, or claims
          arising from your use of the Service, including but not limited to the loss of funds, loss of access to your
          wallet, or any other losses incurred as a result of your use of the Service. You further acknowledge and agree
          that the Service Provider is not responsible for any actions taken by third-party service providers or other
          users of the Service.
        </p>

        <p>
          Some jurisdictions do not allow the exclusion of implied warranties or limitation of liability for incidental
          or consequential damages, which means that some of the above limitations may not apply. In these
          jurisdictions, each party's liability will be limited to the greatest extent permitted by law.
        </p>
      </>
    ),
  },
  {
    title: '"AS IS" and "AS AVAILABLE" Disclaimer',
    content: (
      <>
        <p>
          The Service is provided to You "AS IS" and "AS AVAILABLE" and with all faults and defects without warranty of
          any kind. To the maximum extent permitted under applicable law, the Service Provider, on its own behalf and on
          behalf of its Affiliates and its and their respective licensors and service providers, expressly disclaims all
          warranties, whether express, implied, statutory or otherwise, with respect to the Service, including all
          implied warranties of merchantability, fitness for a particular purpose, title and non-infringement, and
          warranties that may arise out of course of dealing, course of performance, usage or trade practice. Without
          limitation to the foregoing, the Service Provider provides no warranty or undertaking, and makes no
          representation of any kind that the Service will meet Your requirements, achieve any intended results, be
          compatible or work with any other software, applications, systems or services, operate without interruption,
          meet any performance or reliability standards or be error free or that any errors or defects can or will be
          corrected.
        </p>
        <p>
          Without limiting the foregoing, neither the Service Provider nor any of the Service Provider's providers makes
          any representation or warranty of any kind, express or implied: (i) as to the operation or availability of the
          Service, or the information, content, and materials or products included thereon; (ii) that the Service will
          be uninterrupted or error-free; (iii) as to the accuracy, reliability, or currency of any information or
          content provided through the Service; or (iv) that the Service, its servers, the content, or e-mails sent from
          or on behalf of the Service Provider are free of viruses, scripts, trojan horses, worms, malware, timebombs or
          other harmful components.
        </p>
        <p>
          Some jurisdictions do not allow the exclusion of certain types of warranties or limitations on applicable
          statutory rights of a consumer, so some or all of the above exclusions and limitations may not apply to You.
          But in such a case the exclusions and limitations set forth in this section shall be applied to the greatest
          extent enforceable under applicable law.
        </p>
      </>
    ),
  },
  {
    title: 'Governing Law',
    content: (
      <>
        <p>
          The laws of the Country, excluding its conflicts of law rules, shall govern this Terms and Your use of the
          Service. Your use of the Application may also be subject to other local, state, national, or international
          laws.
        </p>
      </>
    ),
  },
  {
    title: 'Disputes Resolution',
    content: (
      <>
        <p>
          If You have any concern or dispute about the Service, You agree to first try to resolve the dispute informally
          by contacting the Service Provider.
        </p>
      </>
    ),
  },
  {
    title: 'For European Union (EU) Users',
    content: (
      <>
        <p>
          If You are a European Union consumer, you will benefit from any mandatory provisions of the law of the country
          in which you are resident in.
        </p>
      </>
    ),
  },
  {
    title: 'United States Legal Compliance',
    content: (
      <>
        <p>
          You represent and warrant that (i) You are not located in a country that is subject to the United States
          government embargo, or that has been designated by the United States government as a "terrorist supporting"
          country, and (ii) You are not listed on any United States government list of prohibited or restricted parties.
        </p>
      </>
    ),
  },
  {
    title: 'Severability and Waiver',
    content: (
      <>
        <h3>Severability</h3>
        <p>
          If any provision of these Terms is held to be unenforceable or invalid, such provision will be changed and
          interpreted to accomplish the objectives of such provision to the greatest extent possible under applicable
          law and the remaining provisions will continue in full force and effect.
        </p>
        <h3>Waiver</h3>
        <p>
          Except as provided herein, the failure to exercise a right or to require performance of an obligation under
          these Terms shall not affect a party's ability to exercise such right or require such performance at any time
          thereafter nor shall the waiver of a breach constitute a waiver of any subsequent breach.
        </p>
      </>
    ),
  },
  {
    title: 'Translation Interpretation',
    content: (
      <>
        <p>
          These Terms and Conditions may have been translated if We have made them available to You on our Service. You
          agree that the original English text shall prevail in the case of a dispute.
        </p>
      </>
    ),
  },
  {
    title: 'Changes to These Terms and Conditions',
    content: (
      <>
        <p>
          We reserve the right, at Our sole discretion, to modify or replace these Terms at any time. If a revision is
          material We will try to provide at least 30 days' notice prior to any new terms taking effect. What
          constitutes a material change will be determined at Our sole discretion.
        </p>
        <p>
          By continuing to access or use Our Service after those revisions become effective, You agree to be bound by
          the revised terms. If You do not agree to the new terms, in whole or in part, please stop using the website
          and the Service.
        </p>
      </>
    ),
  },
  {
    title: 'Contact Us',
    content: (
      <>
        <p>If you have any questions about these Terms and Conditions, You can contact us:</p>
        <ul>
          <li>
            By email:{' '}
            <a href="mailto:hello@adatag.io" target="_blank" rel="noopener noreferrer">
              <b>
                <u>
                  <i>hello@adatag.io</i>
                </u>
              </b>
            </a>
          </li>
        </ul>
      </>
    ),
  },
]
