# 🚢 Freight Insurance Contract (FIC) 🚢

A decentralized insurance solution for shipping and logistics on the Stacks blockchain.

## 📦 Overview

FIC provides automatic insurance coverage for goods during shipping with streamlined claims processing. When shipments are lost or damaged, claims can be filed and processed on-chain, providing transparency and efficiency to the freight insurance process.

## ✨ Features

- 🔒 Create insurance policies for shipments
- 📝 File claims with evidence when goods are lost or damaged
- ✅ Automatic claim processing
- 🚚 Delivery confirmation by receivers
- 📊 Transparent contract statistics

## 🛠️ How It Works

### For Shippers:

1. Create a policy by specifying:
   - Carrier (transport company)
   - Receiver (recipient of goods)
   - Value of goods
   - Duration of coverage (in blocks)

2. Pay the premium (automatically calculated based on value)

3. If goods are lost/damaged, file a claim with evidence

### For Receivers:

1. Confirm delivery when goods arrive safely
2. File claims if goods arrive damaged

### For Contract Owner:

1. Review and approve/reject claims
2. Adjust premium rates as needed
3. Set claim processing timeframes

## 📋 Contract Functions

### Policy Management
- `create-policy`: Create a new insurance policy
- `confirm-delivery`: Mark a shipment as successfully delivered

### Claims Processing
- `file-claim`: Submit a claim for lost or damaged goods
- `approve-claim`: Approve and pay out a claim
- `reject-claim`: Reject a claim with reason

### Administrative
- `set-premium-rate`: Change the premium percentage
- `set-claim-processing-time`: Adjust claim processing duration
- `initialize-counters`: Set up contract counters

### Read-Only
- `get-policy`: View policy details
- `get-claim`: View claim details
- `get-policy-claims-list`: View all claims for a policy
- `get-contract-stats`: View contract statistics

## 🚀 Getting Started

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet)
- [Stacks Wallet](https://www.hiro.so/wallet)

### Deployment
1. Clone this repository
2. Deploy using Clarinet:
```bash
clarinet deploy
```

## 📜 License

MIT

