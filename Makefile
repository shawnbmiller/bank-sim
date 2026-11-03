COBC = cobc -x -free
MODS = ISOCODEC.cob NETIO.cob CRYPTOSM.cob

all: bank atm

bank: BANKSRV.cob $(MODS) ISOMSG.cpy
	$(COBC) -o $@ BANKSRV.cob $(MODS)

atm: ATM.cob $(MODS) ISOMSG.cpy
	$(COBC) -o $@ ATM.cob $(MODS)

clean:
	rm -f bank atm
