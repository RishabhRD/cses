#include <algorithm>
#include <array>
#include <bitset>
#include <chrono>
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

struct custom_hash {
  static uint64_t splitmix64(uint64_t x) {
    x += 0x9e3779b97f4a7c15;
    x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9;
    x = (x ^ (x >> 27)) * 0x94d049bb133111eb;
    return x ^ (x >> 31);
  }

  size_t operator()(uint64_t x) const {
    static const uint64_t FIXED_RANDOM =
        std::chrono::steady_clock::now().time_since_epoch().count();
    return splitmix64(x + FIXED_RANDOM);
  }
};

using ll = long long;
constexpr ll mod = 1e9 + 7;

using safe_set = std::unordered_set<ll, custom_hash>;

template <typename T> using safe_map = std::unordered_map<ll, T>;

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

template <typename T> auto read_matrix(int m, int n) {
  std::vector<std::vector<T>> vec(m, std::vector<T>(n));
  for (int i = 0; i < m; ++i) {
    for (int j = 0; j < n; ++j) {
      std::cin >> vec[i][j];
    }
  }
  return vec;
}

auto solve(ll x, std::vector<ll> const &weight) {
  ll const n = std::size(weight);
  const ll num_subsets = 1 << n;
  std::vector<std::pair<ll, ll>> best(num_subsets, std::pair<ll, ll>{n + 1, 0});
  best[0] = {1, 0};
  for (ll s = 1; s < num_subsets; ++s) {
    for (ll p = 0; p < n; ++p) {
      if (s && (1 << p)) {
        auto option = best[s ^ (1 << p)];
        if (option.second + weight[p] <= x) {
          option.second += weight[p];
        } else {
          ++option.first;
          option.second = weight[p];
        }
        best[s] = std::min(best[s], option);
      }
    }
  }
  return best[num_subsets - 1].first;
}

int main() {
  auto const n = read<ll>();
  auto const x = read<ll>();
  auto const w = read_vec<ll>(n);
  std::cout << solve(x, w) << std::endl;
}
