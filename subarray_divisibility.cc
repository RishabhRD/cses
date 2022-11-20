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

auto const mod(ll a, ll b) { return (a % b + b) % b; }

auto solve(std::vector<ll> const &nums, ll n) {
  std::vector rem(n, 0ll);
  rem[0] = 1;
  ll ans = 0;
  ll cur_sum = 0;
  for (ll i = 0; i < n; ++i) {
    cur_sum += nums[i];
    ll const cur_rem = mod(cur_sum, n);
    ll const target_rem = mod((cur_rem - n), n);
    ans += rem[target_rem];
    ++rem[cur_rem];
  }
  return ans;
}

int main() {
  auto const n = read<ll>();
  auto const nums = read_vec<ll>(n);
  std::cout << solve(nums, n) << std::endl;
}
