# 🚛 Freight Milestone Payments Smart Contract

A Clarity smart contract that enables milestone-based payments for freight deliveries, ensuring truckers get paid at each stage of their delivery journey.

## 🎯 Overview

This smart contract implements a three-milestone payment system for freight deliveries:

- **📦 Pickup Milestone**: Payment released when cargo is picked up
- **🛣️ Halfway Milestone**: Payment released at the halfway point
- **🏁 Delivery Milestone**: Final payment released upon successful delivery

## 🌟 Features

- ✅ Escrow-based payment system with milestone releases
- 🔐 Multi-party authorization (shipper and trucker)
- 📊 Real-time tracking of shipment progress
- 💰 Flexible payment distribution across milestones
- 🚫 Cancellation and refund mechanisms
- 🔍 Comprehensive read-only functions for monitoring

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Basic understanding of Clarity smart contracts

### Installation

1. Clone the repository:
```bash
git clone https://github.com/lookzy98/Freight-Milestone-Payments.git
cd Freight-Milestone-Payments
```

2. Install dependencies:
```bash
npm install
```

3. Check contract syntax:
```bash
clarinet check
```

## 📋 Contract Functions

### Public Functions

#### `create-shipment`
Creates a new shipment with milestone-based payments.

**Parameters:**
- `trucker` (principal): The trucker's wallet address
- `pickup-payment` (uint): Payment amount for pickup milestone
- `halfway-payment` (uint): Payment amount for halfway milestone  
- `delivery-payment` (uint): Payment amount for delivery milestone

**Returns:** Shipment ID

#### `complete-milestone`
Marks a milestone as completed (callable by shipper or trucker).

**Parameters:**
- `shipment-id` (uint): The shipment identifier
- `milestone` (uint): Milestone type (1=pickup, 2=halfway, 3=delivery)

#### `claim-milestone-payment`
Allows trucker to claim payment for completed milestones.

**Parameters:**
- `shipment-id` (uint): The shipment identifier
- `milestone` (uint): Milestone type to claim payment for

#### `cancel-shipment`
Cancels an active shipment and refunds uncompleted milestone payments.

**Parameters:**
- `shipment-id` (uint): The shipment identifier

### Read-Only Functions

- `get-shipment`: Retrieve complete shipment details
- `get-shipment-status`: Get current shipment status
- `get-milestone-status`: Check if specific milestone is completed
- `get-shipment-progress`: View all milestone completion status
- `get-payment-breakdown`: See payment distribution
- `get-shipment-participants`: Get shipper and trucker addresses
- `is-participant`: Check if address is involved in shipment

## 🎮 Usage Examples

### Creating a Shipment

```clarity
;; Shipper creates a $1000 shipment: $300 pickup, $300 halfway, $400 delivery
(contract-call? .freight-milestone-payments create-shipment 
    'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7  ;; trucker address
    u300000000  ;; pickup payment (300 STX in microSTX)
    u300000000  ;; halfway payment  
    u400000000) ;; delivery payment
```

### Completing Milestones

```clarity
;; Complete pickup milestone
(contract-call? .freight-milestone-payments complete-milestone u1 u1)

;; Complete halfway milestone (requires pickup to be done first)
(contract-call? .freight-milestone-payments complete-milestone u1 u2)

;; Complete delivery milestone (requires pickup and halfway)
(contract-call? .freight-milestone-payments complete-milestone u1 u3)
```

### Claiming Payments

```clarity
;; Trucker claims pickup payment
(contract-call? .freight-milestone-payments claim-milestone-payment u1 u1)

;; Trucker claims halfway payment
(contract-call? .freight-milestone-payments claim-milestone-payment u1 u2)

;; Trucker claims final delivery payment
(contract-call? .freight-milestone-payments claim-milestone-payment u1 u3)
```

## 🏗️ Contract Architecture

### Data Structures

**Shipments Map:**
- Shipper and trucker addresses
- Payment amounts for each milestone
- Completion status for each milestone
- Block heights when milestones were completed
- Overall shipment status

**Constants:**
- Milestone identifiers (PICKUP=1, HALFWAY=2, DELIVERY=3)
- Status codes (PENDING=0, ACTIVE=1, COMPLETED=2, CANCELLED=3)
- Error codes for various failure scenarios

### Security Features

- 🔒 Authorization checks ensure only participants can interact
- ⛓️ Sequential milestone completion enforcement
- 💸 Escrow system prevents payment disputes
- 🛡️ Input validation for all parameters

## 🧪 Testing

Run the test suite:
```bash
clarinet test
```

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🎉 Acknowledgments

- Built with ❤️ for the freight and logistics community
- Powered by the Stacks blockchain
- Special thanks to the Clarity development team
