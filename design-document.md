# **Grabbit Auction Design Document**

The Grabbit Auction Protocol in essence is a fully decentralised and on-chain English Auction protocol. 
At a minimum the auction should be:

- **Trust-less**. No individual party can lock up or delay an auction from completion.
- **Contention free**. Multiple bidders must be able to place bids within the same block and not run into concurrency issues.
- **Configurable**. The seller should have the ability to fine tune the auction to their specific needs.

## The Auction Mechanism

The protocol offers 3 different Auctions. Private, Public, and Gated Auctions. The auction mechanism is the same for all 3; however, they differ in how a bidder enrolls into the Auction.

- **Public Auction.** Anyone can enroll themselves into an auction.
- **Private Auction.**  A bidder must first register for an auction. The seller must then enroll the registered bidder into the auction.
- **Gated Auction.** Anyone can enroll themselves into an auction if they hold a token of a certain policy id.

### Common to both **Auctions**

These endpoints are shared between Private, Public and Gated auctions.

- **Announce auction.** A seller can announce an auction by depositing a `Value` to be sold (the "lot") into an auction escrow, attaching the terms of the auction and initialising an empty set of bidders enrolled in the auction. The terms of the auction are:
    - The seller, identified by an `Address`.
    - The beneficiaries - a `Map` of `Address`es and the percentage of the share they will receive
    - The option of who will cover the fees - either the winning bidder or the beneficiaries
    - The lot.
    - The starting time - the earliest time that a bid can be submitted in the auction.
    - The closing time - the earliest time that bids can no longer be submitted in the auction and that the auction closing procedure can begin.
    - The optional extension window and time - the time within which if a bid is made the auction is extended
    - The starting price - the minimum amount of ADA that can be bid in the auction.
    - The buy now price - the amount of ADA needed to automatically purchase the lot and cancel the auction.
    - The minimum bid increment - the minimum difference between a bidder's new bid price and that bidder's previous bid price, defined as the greater of the following:
        - An absolute amount of ADA,
        - A percentage of the previous bid price.
    - The option of a policy id to gate auction participation.
- **Cancel auction.** A seller can cancel an auction and be refunded the lot provided no bidder’s are enrolled into the auction.
- **Bid.** A bidder can replace their previous bid price (if any) with a new bid price, respecting the auction's minimum bid increment and providing sufficient bid deposit, without any interference from anyone else. Bidding can start at the auction's starting time and must cease upon the auction's closing time.
- **Refund.** A bidder can refund and remove themselves from the auction at anytime provided that they can prove the presence of another bid escrow with a higher bid price.
- **Close auction.** Starting at the auction's closing time, anyone can progress the auction's closing procedure:
    - Losing bids are determined in on-chain closing transactions by proving that another bid escrow has a higher bid price.
    - Closing transactions consume losing bid escrows, refunding their bid deposits to their bidders and removing their bidders from the set of enrolled bidders.
    - When only one bidder's node remains in the set, a closing transaction can declare that bidder to be the winner of the auction. The winner receives the auction lot and the winner's bid price is transferred from the bid escrow to the seller, refunding the rest of the bid deposit to the winner.
- **Buy now.** A bidder can pay the full "buy now" amount specified in the auction terms to purchase the lot immediately. If this endpoint is triggered all other bidders are able to refund themselves at any time.

### Private **Auction**

- **Register interest.** A prospective bidder can register interest to participate in an auction, by depositing an amount of ADA (at least the minimum deposit) under a registration escrow:
    - The seller can move the ADA deposit from the registration escrow to a bid escrow that will be under the bidder's exclusive control during the bidding period of the auction.
    - The prospective bidder can recover the ADA deposit if the seller has not moved it into a bid escrow.
- **Enroll bidder.** The seller can enroll a prospective bidder into the auction, which will allow the bidder to submit bids in the auction. To enroll a bidder:
    - Insert a node corresponding to the bidder's public key hash into the auction's set of enrolled bidders. Enrollment fails if the bidder already has a corresponding node in the associate list.
    - Move the ADA deposit from the bidder's registration escrow to a new bid escrow, unique to the auction and the bidder's public key hash.
    - Set the bid price of the bid escrow to "no bid."

### Public & Gated **Auctions**

- **Enroll.** A bidder enrolls themselves into the auction, which will allow the bidder to submit bids in the auction. To enroll a bidder:
    - Insert a node corresponding to the bidder's public key hash into the auction's set of enrolled bidders.
    - Deposit an amount of ADA (at least the minimum deposit for the auction) into a new bid escrow that is unique to the auction and the bidder's public key hash.
    - Set the bid price to "no bid." if it is prior to the auction start with an option of placing a bid once the auction has begun.
    - Include a token with the gate policy id if the auction is gated.

### **Finite set**

A finite set is an on-chain data structure that guarantees uniqueness of its nodes, and allows them to be inserted/removed in transactions that only involve one/two nodes (respectively).

Features:

- Initialise an empty set.
- De-initialise an empty set.
- Insert node - fails if a node already exists in the list with the same key.
- Remove node.
