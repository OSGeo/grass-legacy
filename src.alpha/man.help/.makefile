all: $(dst)
$(dst): $(src)
	rm -f $(dst) ; cp $(src) $(dst) ; chmod +r $(dst)
