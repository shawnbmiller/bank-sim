# COBOL Bank & ATM Simulator

An ATM terminal and an issuer bank host written in GnuCOBOL that talk over a
real TCP socket using ISO 8583 messages.

## Real-world mechanics modelled
- **Transport:** TCP on 127.0.0.1:8583, 2-byte big-endian length framing (libc sockets via `NETIO`).
- **ISO 8583:** MTI + 64-bit hex primary bitmap, fixed/LLVAR/LLLVAR fields (`ISOCODEC`).
  - `0800/0810` network sign-on; `0200/0210` financial request/response.
  - Fields: 2 PAN, 3 processing code (31 balance, 01 withdrawal, 21 deposit), 4 amount,
    7 date/time, 11 STAN, 37 RRN, 39 response code, 41 terminal ID, 49 currency,
    52 PIN block, 54 balance, 64 MAC.
- **PIN security:** ISO 9564 format-0 PIN block, encrypted under a shared terminal key; never sent in clear (`CRYPTOSM`).
- **Message MAC** on every message; bad MAC is rejected.
- **Response codes:** 00 approved, 14 invalid card, 51 insufficient funds, 55 wrong PIN, 61 over limit ($500), 75 PIN tries exceeded (card blocked after 3), 96 system error.

> Cryptography here is a toy stand-in for 3DES/X9.19 HSM operations. Do not use for real security.

## Build & run
Requires GnuCOBOL (`apt install gnucobol`).

    make
    ./bank          # terminal 1 (creates ACCOUNTS.DAT on first run)
    ./atm           # terminal 2

Demo cards: `4111111111111111` PIN `1234` ($10,000); `5500000000000004` PIN `4821` ($250.50).

## Layout
| File | Role |
|---|---|
| `BANKSRV.cob` | Bank host: accounts, PIN verify, authorization |
| `ATM.cob` | ATM UI and message builder |
| `ISOCODEC.cob` / `ISOMSG.cpy` | ISO 8583 pack/unpack/dump |
| `NETIO.cob` | TCP socket layer |
| `CRYPTOSM.cob` | PIN block + MAC |
