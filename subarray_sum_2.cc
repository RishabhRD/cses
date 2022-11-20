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

auto solve(std::vector<ll> const &nums, ll k) {
  auto const n = std::size(nums);
  std::unordered_map<ll, ll> s2;
  s2[0] = 1;
  ll s1 = 0;
  ll ans = 0;
  for (ll i = 0; i < n; ++i) {
    s1 += nums[i];
    auto const target = s1 - k;
    ans += s2[target];
    ++s2[s1];
  }
  return ans;
}

int main() {
  auto const n = read<ll>();
  auto const k = read<ll>();
  auto const nums = read_vec<ll>(n);
  std::cout << solve(nums, k) << std::endl;
}
