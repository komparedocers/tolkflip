#!/bin/bash
# Initialize Cassandra schema

set -e

# Wait for Cassandra to be ready
until cqlsh -e "describe cluster" > /dev/null 2>&1; do
  echo "Waiting for Cassandra to be ready..."
  sleep 2
done

echo "Cassandra is ready! Initializing schema..."

# Execute initialization scripts in order
for script in /docker-entrypoint-initdb.d/*.cql; do
  if [ -f "$script" ]; then
    echo "Executing $script..."
    cqlsh -f "$script"
  fi
done

echo "Cassandra schema initialization complete!"
