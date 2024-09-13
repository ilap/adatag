#!/bin/bash

cd "$(dirname "$0")"

ENV_FILE="../env"
VERSION_FILE="../version"

CMD="docker-compose"
if ! command -v docker-compose &> /dev/null
then
    echo "docker-compose not found, let's try 'docker compose'"
    CMD="docker compose"
fi

$CMD --env-file $ENV_FILE --env-file $VERSION_FILE exec yaci-cli /bin/bash
