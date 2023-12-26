#include <opencv2/core.hpp>
#include <opencv2/core/ocl.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/img_hash.hpp>
#include <hnswlib/hnswlib.h>
#include <iostream>

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "must input the path of image: search_hnsw lena.jpg" << std::endl;
    return -1;
  }

  const int dim = 8;
  const int numNeighbors = 3;

  const std::string hnsw_path = "hnsw.bin";

  cv::ocl::setUseOpenCL(false);

  hnswlib::L2Space space(dim);
  std::unique_ptr<hnswlib::HierarchicalNSW<float>> alg_hnsw(new hnswlib::HierarchicalNSW<float>(&space, hnsw_path));

  cv::Ptr<cv::img_hash::PHash> phash = cv::img_hash::PHash::create();
  auto convf = [](unsigned char a) { return static_cast<float>(a); };

  const cv::Mat input = cv::imread(argv[1]);
  cv::Mat hash;
  phash->compute(input, hash);

  std::cout << "Search: " << hash << std::endl;

  std::vector<float> buf(dim);
  std::transform(hash.datastart, hash.dataend, buf.data(), convf);

  std::priority_queue<std::pair<float, hnswlib::labeltype>> result = alg_hnsw->searchKnn(buf.data(), numNeighbors);

  while (!result.empty()) {
    auto label = result.top();
    std::cout << "label: " << label.second << ", distance: " << label.first << std::endl;
    result.pop();
  }
}
