# -*- coding: utf-8 -*-

# Authors:	Roman Rivera (Invisible Institute)
# individual/complaints-civilian-witnesses_2000-2018_2018-03_18-060-157/clean/src/Makefile

.PHONY: all clean
# Additional files created: clean.log, clean.yaml

all: \
		output/complaints-civilian-witnesses_2000-2018_2018-03.csv.gz

output/complaints-civilian-witnesses_2000-2018_2018-03.csv.gz: \
		src/clean.py \
		input/complaints-civilian-witnesses_2000-2018_2018-03.csv.gz
	python $<


clean:
	rm output/*


# done
