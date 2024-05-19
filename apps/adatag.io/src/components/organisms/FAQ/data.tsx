export const faqData = [
  {
    question: 'What is the cost of minting an adatag?',
    answer:
      "Minting an adatag is completely free, with users only required to pay a transaction fee and a user's claimable time-lock deposit.",
  },
  {
    question: 'What is a time-lock deposit?',
    answer:
      'To prevent abuse of the system, a time-lock deposit feature has been implemented. This feature is designed to prevent users from buying up all very rare usernames, such as the 1-char ones, for nearly free and then selling them at a very high price. When a user mints an adatag, they must deposit some ADA, proportional to the length of the adatag. This deposit is always claimable only by the minter after some time, i.e., the deadline, which is a protocol parameter. The suggested deadline is 20 days. This ensures that users have a stake in the system and are discouraged from abusing it.',
  },
  {
    question: 'How does @Adatag use time-lock deposits, deadlines, deactivation time, and collection time?',
    answer:
      '@Adatag uses time-lock deposits to prevent abuse of the system, deadlines to allow users to claim their deposits after a set period of time, deactivation time to remove the time-lock deposit requirement after a set period of time, and collection time to allow collectors to redeem unclaimed deposits.',
  },
  {
    question: 'What is the purpose of the deadline?',
    answer:
      'The deadline is the time after minting the user can claim their time-lock deposit. It is a configurable parameter that is used for bootstrapping the protocol, and is generally set to 20 days after minting.',
  },
  {
    question: 'What is deactivation time?',
    answer:
      'Deactivation time is the time when the time-lock deposit requirement is no longer in effect. After this time, there is no time-lock deposit requirement at all. It is a configurable parameter at bootstrap time, and is set to approximately 6 months.',
  },
  {
    question: 'What happens to unclaimed time-lock deposits after the collection time?',
    answer:
      'Unclaimed time-lock deposits after the collection time are handled as donations. The collection time is the time when the collector can redeem unclaimed deposits.',
  },
  {
    question:
      'Are the parameters for time-lock deposits, deadlines, deactivation time, and collection time configurable?',
    answer:
      'Yes, these parameters are configurable. The deadline and deactivation time are set at bootstrap time, while the time-lock deposit amount may vary depending on the specific implementation.',
  },
  {
    question: 'What happens if a user does not claim their time-lock deposit before the deadline?',
    answer:
      'If a user does not claim their time-lock deposit before the deadline, the deposit is considered unclaimed and can be redeemed by the collector after the collection time. Any unclaimed deposits after the collection time are handled as donations.',
  },
  {
    question: 'How does adatag work in simple terms?',
    answer:
      "Users can mint their usernames as adatags on the Cardano blockchain for free, with only a transaction fee and a user's claimable time lock deposit, tipicaly claimable after 20 days of minting.",
  },
  {
    question: 'Is there a reservation process for adatags?',
    answer: 'No, there is no reservation process for adatags. Users can mint adatags directly.',
  },
  {
    question: 'What kind of characters can be used in minted usernames (adatags)?',
    answer:
      'Minted usernames (adatags) are limited to a maximum of 16 characters and must start with a letter and end with a letter or digit. They can contain lowercase letters, digits, and the dash, underscore and period symbols (-, _, .).',
  },
  {
    question: 'Are minted usernames (adatags) case-sensitive?',
    answer: 'No, minted usernames (adatags) are not case-sensitive. They are treated as lowercase.',
  },
  {
    question: 'What is the maximum length of a minted username (adatag)?',
    answer: 'The maximum length of a minted username (adatag) is 16 characters.',
  },
  {
    question: 'Can minted usernames (adatags) be sold on any Cardano NFT marketplace?',
    answer: 'Yes, minted usernames (adatags) can be sold on any Cardano NFT marketplace.',
  },
  {
    question: 'Can minted usernames (adatags) be used or bought from an exchange?',
    answer:
      'No, minted usernames (adatags) cannot be used or bought from an exchange. They are minted directly on the Cardano blockchain.',
  },
  {
    question: 'How long before users can start using their minted usernames (adatags)?',
    answer:
      'Users can start using their minted usernames (adatags) immediately after minting, but the claimable time lock deposit is usually available for claiming after 20 days.',
  },
  {
    question: 'What are the future plans for the adatag project?',
    answer:
      'The future plans for the adatag project include maintaining its open-source nature, expanding its features, and enhancing its integration with the Cardano ecosystem.',
  },
  {
    question: 'Why are some characters not allowed in minted usernames (adatags)?',
    answer:
      'Some characters are not allowed in adatags to ensure compatibility with client side apps and to align with popular social media platforms.',
  },
  {
    question: 'What is the purpose of the adatag project?',
    answer:
      'The purpose of the adatag project is to provide users with a decentralized solution for creating and managing minted usernames on the Cardano blockchain.',
  },
]
