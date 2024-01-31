import { component$ } from "@builder.io/qwik";
import {
  QwikCityProvider,
  RouterOutlet,
  ServiceWorkerRegister,
} from "@builder.io/qwik-city";
import { RouterHead } from "./components/router-head/router-head";


import "./global.css";


/*import * as T from 'translucent-cardano'
import * as C from '@adatag/shared/config'
import * as TU from '@adatag/shared/test-utils'
import * as P from '@adatag/shared/plutus'
import * as U from '@adatag/shared/utils'
import * as I from '@adatag/integri-tree'


export function stringifyData(data: unknown) {
  return JSON.stringify(
    data,
    (key, value) => (typeof value === 'bigint' ? value.toString() : value),
    '  ',
  )
}

console.log(`############################### T: ${stringifyData(T)}`);
console.log(`############################### C: ${stringifyData(C)}`);
console.log(`############################### I: ${stringifyData(I)}`);
console.log(`############################### TU: ${stringifyData(TU)}`);
console.log(`############################### P: ${stringifyData(P)}`);
console.log(`############################### U: ${stringifyData(U)}`);

*/

export default component$(() => {
  /**
   * The root of a QwikCity site always start with the <QwikCityProvider> component,
   * immediately followed by the document's <head> and <body>.
   *
   * Don't remove the `<head>` and `<body>` elements.
   */
  return (
    <QwikCityProvider>
      <head>
        <meta charSet="utf-8" />
        <link rel="manifest" href="/manifest.json" />
        <RouterHead />
        <ServiceWorkerRegister />
      </head>
      <body lang="en">
        <RouterOutlet />
      </body>
    </QwikCityProvider>
  );
});
