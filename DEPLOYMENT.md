# Tolkflip Deployment Guide

## Table of Contents
1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Local Development Setup](#local-development-setup)
4. [Production Deployment](#production-deployment)
5. [Monitoring and Maintenance](#monitoring-and-maintenance)
6. [Troubleshooting](#troubleshooting)

## Overview

Tolkflip is a cloud-agnostic, multilingual chat application built with microservices architecture. This guide covers deployment to various environments.

### Architecture Components

**Backend Services:**
- API Gateway (Node.js/Express)
- Authentication Service (Node.js with Twilio)
- Chat Service (Node.js with Socket.IO)
- User Service (Node.js)
- Translation Service (Python with MarianNMT)
- Transcription Service (Python with Whisper)
- Media Service (Node.js with MinIO)
- Presence Service (Node.js with Redis)

**Databases:**
- Cassandra (NoSQL for messages and user data)
- Redis (Caching and presence)
- MinIO (Object storage for media)

**Mobile Apps:**
- Android (Java)
- iOS (Swift)

## Prerequisites

### Required Software
- Docker 20.10+
- Kubernetes 1.25+ (kubectl)
- Helm 3.0+ (optional but recommended)
- Node.js 18+
- Python 3.10+
- Java 17+ (for Android)
- Xcode 15+ (for iOS)

### Cloud Provider Accounts (for production)
- AWS/GCP/Azure account
- Twilio account (for SMS verification)
- Firebase account (for push notifications)

## Local Development Setup

### 1. Clone the Repository

```bash
git clone https://github.com/yourusername/tolkflip.git
cd tolkflip
```

### 2. Environment Configuration

Create a `.env` file in the root directory:

```env
# Twilio Configuration
TWILIO_ACCOUNT_SID=your_account_sid
TWILIO_AUTH_TOKEN=your_auth_token
TWILIO_PHONE_NUMBER=your_phone_number

# JWT Secrets
JWT_SECRET=your-jwt-secret-change-in-production
JWT_REFRESH_SECRET=your-refresh-secret-change-in-production

# Cassandra
CASSANDRA_HOSTS=cassandra

# Redis
REDIS_HOST=redis
REDIS_PORT=6379

# MinIO
MINIO_ENDPOINT=minio
MINIO_PORT=9000
MINIO_ACCESS_KEY=minioadmin
MINIO_SECRET_KEY=minioadmin
```

### 3. Start with Docker Compose

```bash
# Start all services
docker-compose up -d

# View logs
docker-compose logs -f

# Stop all services
docker-compose down
```

### 4. Initialize Cassandra Schema

Wait for Cassandra to be ready (30-60 seconds), then:

```bash
docker exec -it tolkflip-cassandra cqlsh

# The schema will be auto-initialized by the auth service on first run
# Or manually run:
docker-compose restart auth-service
```

### 5. Access Services

- API Gateway: http://localhost:3000
- Prometheus: http://localhost:9090
- Grafana: http://localhost:3001 (admin/admin)
- MinIO Console: http://localhost:9001 (minioadmin/minioadmin)

## Production Deployment

### Option 1: Kubernetes Deployment

#### 1. Setup Kubernetes Cluster

**AWS EKS:**
```bash
eksctl create cluster \
  --name tolkflip-cluster \
  --region us-east-1 \
  --nodegroup-name standard-workers \
  --node-type t3.xlarge \
  --nodes 3 \
  --nodes-min 3 \
  --nodes-max 10 \
  --managed
```

**GCP GKE:**
```bash
gcloud container clusters create tolkflip-cluster \
  --zone us-central1-a \
  --num-nodes 3 \
  --machine-type n1-standard-4 \
  --enable-autoscaling \
  --min-nodes 3 \
  --max-nodes 10
```

#### 2. Create Kubernetes Secrets

```bash
kubectl create namespace tolkflip

kubectl create secret generic tolkflip-secrets \
  --from-literal=JWT_SECRET=your-jwt-secret \
  --from-literal=JWT_REFRESH_SECRET=your-refresh-secret \
  --from-literal=TWILIO_ACCOUNT_SID=your-sid \
  --from-literal=TWILIO_AUTH_TOKEN=your-token \
  --from-literal=TWILIO_PHONE_NUMBER=your-phone \
  --from-literal=MINIO_ACCESS_KEY=your-access-key \
  --from-literal=MINIO_SECRET_KEY=your-secret-key \
  -n tolkflip
```

#### 3. Deploy Infrastructure

```bash
# Apply namespace and config
kubectl apply -f infrastructure/kubernetes/namespace.yaml
kubectl apply -f infrastructure/kubernetes/configmap.yaml

# Deploy StatefulSets (Cassandra, Redis)
kubectl apply -f infrastructure/kubernetes/statefulsets/

# Wait for StatefulSets to be ready
kubectl wait --for=condition=ready pod -l app=cassandra -n tolkflip --timeout=300s

# Deploy services
kubectl apply -f infrastructure/kubernetes/deployments/

# Verify deployment
kubectl get pods -n tolkflip
kubectl get services -n tolkflip
```

#### 4. Setup Ingress (Optional)

```bash
# Install NGINX Ingress Controller
kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/controller-v1.8.1/deploy/static/provider/cloud/deploy.yaml

# Apply ingress rules
kubectl apply -f infrastructure/kubernetes/ingress.yaml
```

### Option 2: Docker Swarm Deployment

```bash
# Initialize swarm
docker swarm init

# Deploy stack
docker stack deploy -c docker-compose.yml tolkflip

# Check services
docker service ls

# Scale services
docker service scale tolkflip_chat-service=10
```

## Monitoring and Maintenance

### Prometheus Metrics

Access Prometheus at http://your-domain:9090

**Important Metrics:**
- `http_request_duration_seconds` - API response times
- `websocket_connections_total` - Active WebSocket connections
- `translation_requests_total` - Translation service usage
- `cassandra_read_latency` - Database performance

### Grafana Dashboards

Access Grafana at http://your-domain:3000

Pre-configured dashboards:
1. **Service Overview** - Overall system health
2. **Chat Performance** - WebSocket and messaging metrics
3. **Translation Analytics** - Language usage and translation stats
4. **Database Performance** - Cassandra and Redis metrics

### Log Aggregation

**Using ELK Stack:**

```bash
# Deploy ELK stack
kubectl apply -f infrastructure/monitoring/elk/

# Access Kibana
kubectl port-forward svc/kibana 5601:5601 -n tolkflip
```

### Backup Strategy

**Cassandra Backup:**
```bash
# Create snapshot
docker exec tolkflip-cassandra nodetool snapshot tolkflip

# Backup to S3
docker exec tolkflip-cassandra tar -czf /tmp/cassandra-backup.tar.gz \
  /var/lib/cassandra/data/tolkflip

aws s3 cp /tmp/cassandra-backup.tar.gz s3://your-backup-bucket/
```

**Redis Backup:**
```bash
# Redis automatically creates dump.rdb
docker exec tolkflip-redis redis-cli BGSAVE

# Copy to backup location
docker cp tolkflip-redis:/data/dump.rdb ./backup/
```

## Mobile App Deployment

### Android

1. **Build Release APK:**
```bash
cd mobile/android
./gradlew assembleRelease
```

2. **Upload to Google Play:**
- Sign the APK with your keystore
- Upload to Google Play Console
- Fill in app details and screenshots
- Submit for review

### iOS

1. **Build for App Store:**
```bash
cd mobile/ios
xcodebuild -workspace Tolkflip.xcworkspace \
  -scheme Tolkflip \
  -configuration Release \
  -archivePath build/Tolkflip.xcarchive \
  archive
```

2. **Upload to App Store:**
- Use Xcode or fastlane to upload to App Store Connect
- Submit for TestFlight beta testing
- Submit for App Store review

## Troubleshooting

### Common Issues

**1. Cassandra Not Starting:**
```bash
# Check logs
docker logs tolkflip-cassandra

# Increase memory if needed
# Edit docker-compose.yml and increase resources
```

**2. WebSocket Connection Failures:**
```bash
# Check chat service logs
kubectl logs -f deployment/chat-service -n tolkflip

# Verify Redis is running
kubectl get pods -l app=redis -n tolkflip
```

**3. Translation Service Slow:**
```bash
# Check GPU availability
nvidia-smi

# Scale up translation service
kubectl scale deployment translation-service --replicas=5 -n tolkflip
```

**4. High Memory Usage:**
```bash
# Check resource usage
kubectl top pods -n tolkflip

# Adjust resource limits in deployment files
```

### Health Checks

```bash
# Check all services
curl http://localhost:3000/health
curl http://localhost:3001/health  # Auth
curl http://localhost:3003/health  # Chat
curl http://localhost:3004/health  # Translation
```

### Performance Tuning

**Cassandra:**
- Increase heap size for production: `-Xms4G -Xmx4G`
- Enable JMX monitoring
- Configure compaction strategy

**Redis:**
- Set maxmemory policy: `maxmemory-policy allkeys-lru`
- Enable persistence: `appendonly yes`

**Translation Service:**
- Use GPU for faster inference
- Load models into memory at startup
- Implement caching for frequent translations

## Security Checklist

- [ ] Change all default passwords
- [ ] Enable SSL/TLS for all services
- [ ] Configure firewall rules
- [ ] Enable authentication for Prometheus/Grafana
- [ ] Implement rate limiting
- [ ] Regular security updates
- [ ] Enable audit logging
- [ ] Backup encryption keys
- [ ] Configure CORS properly
- [ ] Use secrets management (AWS Secrets Manager, Vault)

## Scaling Guidelines

### Horizontal Scaling

**Chat Service:**
- Scale based on active connections
- Target: 1000 connections per instance
- Use session affinity for WebSocket

**Translation Service:**
- Scale based on queue depth
- Consider GPU instances for production
- Cache translations aggressively

**Cassandra:**
- Add nodes for increased capacity
- Replication factor: 3 for production
- Use NetworkTopologyStrategy

### Vertical Scaling

**Memory Requirements:**
- API Gateway: 512MB - 1GB
- Chat Service: 1GB - 2GB
- Translation Service: 4GB - 8GB (with models loaded)
- Cassandra: 8GB - 16GB per node
- Redis: 2GB - 4GB

## Support and Maintenance

**Regular Tasks:**
- Weekly: Review metrics and logs
- Monthly: Update dependencies
- Quarterly: Security audit
- Yearly: Disaster recovery drill

**Monitoring Alerts:**
- High error rate (>5%)
- High latency (>2s p95)
- Low disk space (<20%)
- Service down
- Database connection pool exhausted

For issues and support, visit: https://github.com/yourusername/tolkflip/issues
