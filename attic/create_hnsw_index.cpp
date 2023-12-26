#include <opencv2/core.hpp>
#include <opencv2/core/ocl.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/img_hash.hpp>
#include <hnswlib/hnswlib.h>
#include <iostream>

int main(int argc, char **argv) {
  const int dim = 8;
  const int M = 16;
  const int ef_construction = 200;  // Controls index search speed/build speed trade-off
  const int max_elements = 100000;
  const std::string hnsw_path = "hnsw.bin";

  cv::ocl::setUseOpenCL(false);

  hnswlib::L2Space space(dim);
  std::unique_ptr<hnswlib::HierarchicalNSW<float>> alg_hnsw(new hnswlib::HierarchicalNSW<float>(&space, max_elements, M, ef_construction));

  cv::Ptr<cv::img_hash::PHash> phash = cv::img_hash::PHash::create();
  auto convf = [](unsigned char a) { return static_cast<float>(a); };
  std::vector<float> buf(dim);

  for (int i = 1; i < argc; ++i) {
    const cv::Mat input = cv::imread(argv[i]);
    cv::Mat hash;
    phash->compute(input, hash);

    std::cout << i << ":" << argv[i] << " => " << hash << std::endl;
    std::transform(hash.datastart, hash.dataend, buf.data(), convf);

    alg_hnsw->addPoint(buf.data(), i);
  }

  alg_hnsw->saveIndex(hnsw_path);
}
