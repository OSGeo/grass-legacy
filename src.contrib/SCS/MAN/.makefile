all: $(cat)
$(cat): $(man) man.header
	tbl -TX man.header $(man) | nroff -Tlp | col -b > $(cat)
