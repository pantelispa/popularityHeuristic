
all: popSearch ranSearch popSearch_unpredictable ranSearch_unpredictable popSearch_RanConc popSearch_RanDisp

# we should probably enable setting arguments
# like noAgents, noAlts, noCores etc

popSearch: popSearch.R
	R CMD BATCH popSearch.R

ranSearch: ranSearch.R
	R CMD BATCH ranSearch.R

popSearch_unpredictable: popSearch_unpredictable.R
	R CMD BATCH popSearch_unpredictable.R

ranSearch_unpredictable: ranSearch_unpredictable.R
	R CMD BATCH ranSearch_unpredictable.R

popSearch_RanConc: popSearch_RanConc.R
	R CMD BATCH popSearch_RanConc.R

popSearch_RanDisp: popSearch_RanDisp.R
	R CMD BATCH popSearch_RanDisp.R
