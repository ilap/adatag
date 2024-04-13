# Usage

1. run `nx run chain:spin-up:int` from ${workspaceRoot} to spin-up Yaci-devkit based custom network
2. run `bun test -m adatag-minting --env-file ./.env.test.int --timeout 600000`
3. Gather the current minting policy id from http://localhost:5173/transactions
3. in other terminal run `cd apps/fetch-test && bun run src/index.tsx`