# -*- coding: utf-8 -*-

# Authors:	Roman Rivera (Invisible Institute)
# individual/complaints-complainants_1967-1999_2016-12_/clean/src/Makefile

.PHONY: all clean
# Additional files created: clean.log, clean.yaml

all: \
		output/complaints-complainants_1967-1999_2016-12.csv.gz

output/complaints-complainants_1967-1999_2016-12.csv.gz: \
		src/clean.py \
		input/complaints-complainants_1967-1999_2016-12.csv.gz
	python $<


clean:
	rm output/*


# done
