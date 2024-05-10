ENV=int
#cd contracts/aiken && aiken fmt && cd -
#nx run aiken:build:dev --skipNxCache
nx run chain:spin-up:${ENV} --skipNxCache
cd libs/deployment-ts && bun test mock-adahandle adatag-minting --env-file ../../.env.test.${ENV} --timeout 60000
cd ../../apps/adatag.io && bun sync
