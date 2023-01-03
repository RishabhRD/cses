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

auto solve(std::vector<ll> const &nums) {
  auto const n = std::size(nums);
  auto const max = *std::max_element(std::begin(nums), std::end(nums));
  std::vector<ll> cnt(max + 1, 0);
  for (auto n : nums)
    ++cnt[n];
  std::vector<std::vector<ll>> prime_factors(max + 1);
  for (ll i = 2; i <= max; ++i) {
    if (prime_factors[i].size() == 0) {
      for (ll j = i; j <= max; j += i) {
        prime_factors[j].push_back(i);
      }
    }
  }
  std::vector<ll> num_divisible_by(max + 1);
  std::vector<ll> num_prime_divisors(max + 1);
  for (ll i = 0; i < n; ++i) {
    auto const primes = prime_factors[nums[i]];
    for (ll mask = 1; mask < (1 << primes.size()); ++mask) {
      std::bitset<32> bs(mask);
      ll num = 1;
      for (ll j = 0; j < primes.size(); ++j) {
        if (bs[j])
          num *= primes[j];
      }
      ++num_divisible_by[num];
      num_prime_divisors[num] = bs.count();
    }
  }
  ll sum = 0;
  for (ll i = 1; i <= max; ++i) {
    ll const w = (num_divisible_by[i] * (num_divisible_by[i] - 1)) / 2;
    if (num_prime_divisors[i] % 2) {
      sum += w;
    } else {
      sum -= w;
    }
  }
  std::cout << ((n * (n - 1)) / 2) - sum << std::endl;
}

int main() {
  auto const n = read<ll>();
  auto const nums = read_vec<ll>(n);
  solve(nums);
}
