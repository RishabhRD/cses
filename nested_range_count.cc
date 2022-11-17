#include <algorithm>
#include <array>
#include <bitset>
#include <cmath>
#include <deque>
#include <iostream>
#include <iterator>
#include <limits>
#include <map>
#include <numeric>
#include <optional>
#include <queue>
#include <set>
#include <stack>
#include <unordered_map>
#include <unordered_set>
#include <vector>

template <typename T> T read() {
  T t;
  std::cin >> t;
  return t;
}

template <typename T> std::vector<T> read_vec(int n) {
  std::vector<T> vec(n);
  for (auto &ele : vec)
    std::cin >> ele;
  return vec;
}

template <typename T> std::vector<T> read_matrix(int m, int n) {
  std::vector<std::vector<T>> vec(m, std::vector<T>(n));
  for (int i = 0; i < m; ++i) {
    for (int j = 0; j < n; ++j) {
      std::cin >> vec[i][j];
    }
  }
  return vec;
}

struct range {
  int low;
  int high;
  int idx;
};

struct ans_t {
  int ans;
  int idx;
};

using ll = long long;

void merge(std::vector<range> &nums, std::vector<ans_t> &ans, int low, int mid,
           int high) {
  int j = mid + 1;
  for (int i = low; i <= mid; ++i) {
    while (j <= high and nums[i].high >= nums[j].high) {
      ++j;
    }
    ans[nums[i].idx].ans += j - mid - 1;
  }
  std::sort(std::begin(nums) + low, std::begin(nums) + high + 1,
            [](auto a, auto b) { return a.high < b.high; });
}

void num_ele_less_helper(std::vector<range> &nums, std::vector<ans_t> &ans,
                         int low, int high) {
  if (low < high) {
    int mid = (low + high) / 2;
    num_ele_less_helper(nums, ans, low, mid);
    num_ele_less_helper(nums, ans, mid + 1, high);
    merge(nums, ans, low, mid, high);
  }
}

auto num_ele_less(std::vector<range> nums) {
  std::vector<ans_t> ans;
  for (int i = 0; i < std::size(nums); ++i) {
    ans.push_back({0, i});
  }
  num_ele_less_helper(nums, ans, 0, std::size(nums) - 1);
  return ans;
}

void merge_prime(std::vector<range> &nums, std::vector<ans_t> &ans, int low,
                 int mid, int high) {
  int i = low;
  for (int j = mid + 1; j <= high; ++j) {
    while (i <= mid and nums[i].high >= nums[j].high) {
      ++i;
    }
    ans[nums[j].idx].ans += i - low;
  }
  std::sort(std::begin(nums) + low, std::begin(nums) + high + 1,
            [](auto a, auto b) { return a.high > b.high; });
}

void num_ele_greater_helper(std::vector<range> &nums, std::vector<ans_t> &ans,
                            int low, int high) {
  if (low < high) {
    int mid = (low + high) / 2;
    num_ele_greater_helper(nums, ans, low, mid);
    num_ele_greater_helper(nums, ans, mid + 1, high);
    merge_prime(nums, ans, low, mid, high);
  }
}

auto num_ele_greater(std::vector<range> nums) {
  std::vector<ans_t> ans;
  for (int i = 0; i < std::size(nums); ++i) {
    ans.push_back({0, i});
  }
  num_ele_greater_helper(nums, ans, 0, std::size(nums) - 1);
  return ans;
}

auto solve(std::vector<std::pair<int, int>> const &ranges_) {
  auto const n = std::size(ranges_);
  std::vector<range> ranges(n);
  for (int i = 0; i < n; ++i) {
    ranges[i] = {ranges_[i].first, ranges_[i].second, i};
  }
  std::sort(std::begin(ranges), std::end(ranges), [](auto a, auto b) {
    if (a.low == b.low)
      return a.high > b.high;
    return a.low < b.low;
  });
  auto first_ans = num_ele_less(ranges);
  auto second_ans = num_ele_greater(ranges);
  for (auto [e, _] : first_ans) {
    std::cout << e << ' ';
  }
  std::cout << std::endl;
  for (auto [e, _] : second_ans) {
    std::cout << e << ' ';
  }
  std::cout << std::endl;
}

int main() {
  auto const n = read<int>();
  std::vector<std::pair<int, int>> nums;
  for (int i = 0; i < n; ++i) {
    auto const a = read<int>();
    auto const b = read<int>();
    nums.push_back({a, b});
  }
  solve(nums);
}
