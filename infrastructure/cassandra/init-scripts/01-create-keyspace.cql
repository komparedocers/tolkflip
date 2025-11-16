-- Create Tolkflip keyspace
CREATE KEYSPACE IF NOT EXISTS tolkflip
  WITH replication = {
    'class': 'NetworkTopologyStrategy',
    'datacenter1': 3
  }
  AND durable_writes = true;

USE tolkflip;
