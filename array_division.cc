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

using ll = long long;

template <typename Predicate>
ll binary_search(ll low, ll high, Predicate &&is_possible) {
  if (low == high)
    return low;
  auto const mid = low + ((high - low) / 2);
  if (is_possible(mid)) {
    return binary_search(mid + 1, high, is_possible);
  } else {
    return binary_search(low, mid, is_possible);
  }
}

bool can_be_maximum(std::vector<ll> const &nums, ll max, ll k) {
  ll cnt = 1;
  ll sum = 0;
  for (auto const ele : nums) {
    sum += ele;
    if (sum > max) {
      ++cnt;
      sum = ele;
    }
  }
  return cnt <= k;
}

auto solve(std::vector<ll> const &nums, ll k) {
  auto const low = *std::max_element(std::begin(nums), std::end(nums));
  return binary_search(low, INT64_MAX,
                       [&](auto i) { return !can_be_maximum(nums, i, k); });
}

int main() {
  auto const n = read<ll>();
  auto const k = read<ll>();
  auto const nums = read_vec<ll>(n);
  std::cout << solve(nums, k) << std::endl;
}
