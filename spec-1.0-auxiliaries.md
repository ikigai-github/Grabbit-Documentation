# Auxiliary Scripts
These are scripts which are not core to the auction protocol, but which we developed as part of the development of Grabbit for a variety of other reasons. They are generally standalone, except for a few cross-dependencies around the Launchpad and Merkle Tree contracts.

All files are found from root with the path `/onchain/src/Contracts/Onchain/`

## Bulk Mint
`...Onchain/Collections/BulkMint.hs`

Purpose: Mint fixed size collection of tokens (supports burning).

- Minting requires the spending of a UTxO with a TxOutRef (which is a parameter to the script).   
- Exactly collectionSize (from the BulkMintParameters) tokens must be minted when minting.  
- You cannot mint and burn in the same transaction.  
- Tokens minted by this policy cannot be burned in transactions where they are being unlocked from scripts.  

### Datatypes

```hs

data BulkMintParametersD = BulkMintParametersD
  { uniqueRef :: TxOutRef
  , collectionSize :: Integer
  }
  
data BulkMintAction
  = MintBulk
  | BurnBulk
```

## Direct Transfer  
`...Onchain/Collections/DirectTransfer.hs`

Purpose: Facilitate P2P trading of assets, create, update, and accept trade offers. 

---
### Datatypes

```hs

data DirectOfferDatum (s :: S)
  = DirectOfferDatum 
      { creator :: Address
      , toBuy :: Value 
      }
    
data DirectOfferRedeemer
  = Accept
  | Update
  | Cancel
```
---

### Accept Endpoint

Inputs: 
- From `ownHash`:
 - The value being offered 
 - `DirectOfferDatum {address := offerCreatorsAddress, toBuy := desiredAssets}`
- From anywhere:
 - Anything
 
Outputs:
- To `datum.creator` 
  - `value >= datum.toBuy`
- To `ownHash`
  - Nothing
  
Validation Rules:
- signed by `offeree` 

---
### Update Endpoint

Inputs: 
- From `ownHash`:
 - The value being offered 
 - `DirectOfferDatum {address := offerCreatorsAddress, toBuy := desiredAssets}` 
- From anywhere:
 - Anything
 
Outputs:
- To `datum.creator` 
  - `value >= datum.toBuy`
- To `ownHash`
  - Nothing
  
Validation Rules:
- signed by `ownInput.datum.creator` 

---
### Cancel Endpoint

Inputs: 
- From `ownHash`:
 - The value being offered 
 - `DirectOfferDatum {address := offerCreatorsAddress, toBuy := desiredAssets}` 
- From anywhere:
 - Anything
 
Outputs:
- To `datum.creator` 
  - `value >= datum.toBuy`
- To `ownHash`
  - Nothing
  
Validation Rules:
- Either
  - signed by `ownInput.datum.creator`
- Or
  - `
    length (filter (\txo -> txOutAddress txo == datum.creator && txo.value #>= ownInput.value) (txInfoOutputs info)) == 1 
    `
      
## Inactive Auxiliaries
These scripts are inactive for various reasons. None were included in the audit done by CertiK.

### Merkle Tree
`...Onchain/Collections/MerkleTree.hs`

A Plutarch v1.2 implementation of a Merkle Tree. I believe Anastasia Labs' [Plutarch-Merkle-Tree](https://github.com/Anastasia-Labs/plutarch-merkle-tree) was based on this implementation, and should be more up to date and better maintained. I would recommend using Anastasia's over ours for now.

### Onchain FT Mint
`...Onchain/Collections/OnchainFTMint.hs`

A simple oneshot minting & burning policy. In order to mint, you must consume a specific UTxO parameterized into the script.

### Sequential Mint
`...Onchain/Collections/SequentialMint.hs`

The sequential minting system includes 2 minting policies and 1 validator which allows the "sequence owner" to mint up to a certain number of NFTs in a series of transactions.

### NFT Launchpad
`...Onchain/Metadata/NFTLaunchpad.hs`

The NFT Launchpad itself includes a NFT minting policy modeled after the Sequential minting policy, with checks for CIP-68 & CIP-102 minting.
It also includes a CIP-68 NFT evolvability validator for hosting the reference tokens of the NFTs minted.

#### Launchpad Threads
`...Onchain/Metadata/NFTLaunchpadThreads.hs`

A state threading system used to support the sequential minting of the NFT launchpad.