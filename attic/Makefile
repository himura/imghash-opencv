TARGETS=opencv hnswlib create_hnsw_index search_hnsw

all: $(TARGETS)

%: %.cpp
	g++ $< `pkg-config opencv4 --cflags --libs` -o $@

.PHONY: clean
clean:
	rm -f $(TARGETS)

opencv: opencv.cpp
hnswlib: hnswlib.cpp
create_hnsw_index: create_hnsw_index.cpp
