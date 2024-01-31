import { component$, useStore } from '@builder.io/qwik'
import { QwikLogo } from '../icons/qwik'
import styles from './header.module.css'

import { Translucent, Maestro } from 'translucent-cardano'

export function stringifyData(data: unknown) {
  return JSON.stringify(
    data,
    (key, value) => (typeof value === 'bigint' ? value.toString() : value),
    '  ',
  )
}

export default component$(() => {
  /*const store = useStore({ api: {}});
  useClientEffect(async () => {
    // some where in your client code
    const api = window.cardano.nami.enable();
  // Assumes you are in a browser environment
    translucent.selectWallet(await api);
    console.log(window);
  });
*/
  return (
    <header class={styles.header}>
      <div class={['container', styles.wrapper]}>
        <div class={styles.logo}>
          <a href="/" title="qwik">
            <QwikLogo height={50} width={143} />
          </a>
        </div>
        <ul>
          <li>
            <button
              onClick$={async () => {
                const apiKey = 'dxH6GrWMw75wn0xWF1SbPycdqztVVTLf'
                console.log(`### Before meastro`)
                const maestro = new Maestro({
                  network: 'Preview',
                  apiKey: apiKey,
                })
                console.log(`#### After maestro`)
                const translucent = await Translucent.new(maestro, 'Preview')

                console.log(`Translucent ${stringifyData(maestro)}`)

                const api = window.cardano.lace.enable()
                // Assumes you are in a browser environment
                translucent.selectWallet(await api)
                console.log(`@@@@@@@ API:\n ${stringifyData(await api)}`)
                console.log(
                  `@@@@@@@ Cardano:\n ${stringifyData(window.cardano)}`,
                )
              }}
            >
              Connect to Wallet{' '}
            </button>
          </li>
        </ul>
      </div>
    </header>
  )
})
