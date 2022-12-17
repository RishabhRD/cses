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

template <typename T, typename GroupFunc> class segment_tree {
  GroupFunc func;
  ll n;
  std::vector<T> arr;

public:
  segment_tree(std::vector<T> const &arr, GroupFunc func)
      : func(std::move(func)), n(std::size(arr)), arr(n * 4) {
    build(1, 0, n - 1, arr);
  }

  T query(ll tl, ll tr) { return query(1, 0, n - 1, tl, tr); }

  void update(ll i, T const &val) {
    update(i, [val](auto e) { return val; });
  }

  template <typename F> void update(ll i, F &&f) { update(1, 0, n - 1, i, f); }

private:
  void build(ll i, ll l, ll r, std::vector<T> const &vec) {
    if (l == r) {
      arr[i] = vec[l];
    } else {
      auto const m = l + ((r - l) / 2);
      build(2 * i, l, m, vec);
      build(2 * i + 1, m + 1, r, vec);
      arr[i] = func(arr[2 * i], arr[2 * i + 1]);
    }
  }

  T query(ll i, ll l, ll r, ll tl, ll tr) {
    if (l == tl && r == tr)
      return arr[i];
    auto const m = l + ((r - l) / 2);
    if (tl >= l and tr <= m) {
      return query(2 * i, l, m, tl, tr);
    } else if (tl >= (m + 1) and tr <= r) {
      return query(2 * i + 1, m + 1, r, tl, tr);
    } else {
      auto const left = query(2 * i, l, m, tl, m);
      auto const right = query(2 * i + 1, m + 1, r, m + 1, tr);
      return func(left, right);
    }
  }

  template <typename F> void update(ll i, ll l, ll r, ll idx, F &&f) {
    if (l == r) {
      arr[i] = f(arr[i]);
      return;
    }
    auto const m = l + ((r - l) / 2);
    if (idx <= m) {
      update(2 * i, l, m, idx, f);
    } else {
      update(2 * i + 1, m + 1, r, idx, f);
    }
    arr[i] = func(arr[2 * i], arr[2 * i + 1]);
  }
};

template <typename T, typename GroupFunc>
segment_tree(std::vector<T> const &, GroupFunc) -> segment_tree<T, GroupFunc>;

struct node {
  ll sum;
  ll max_prefix_sum;
  ll max_suffix_sum;
  ll max_sum;
};

auto solve(std::vector<ll> const &nums,
           std::vector<std::pair<ll, ll>> const &queries) {
  auto const n = std::size(nums);
  auto const build_func = [](node a, node b) {
    auto const sum = a.sum + b.sum;
    auto const max_prefix_sum =
        std::max({a.sum + b.max_prefix_sum, a.max_prefix_sum});
    auto const max_suffix_sum =
        std::max({b.max_suffix_sum, b.sum + a.max_suffix_sum});
    auto const max_sum =
        std::max({a.max_sum, b.max_sum, a.max_suffix_sum + b.max_prefix_sum});
    return node{sum, max_prefix_sum, max_suffix_sum, max_sum};
  };
  std::vector<node> temp;
  for (auto n : nums)
    temp.push_back({n, n, n, n});
  segment_tree stree(temp, build_func);
  for (auto [k_, v_] : queries) {
    auto const k = k_;
    auto const v = v_;
    stree.update(k, [v](auto _) { return node{v, v, v, v}; });
    std::cout << std::max(0ll, stree.query(0, n - 1).max_sum) << std::endl;
  }
}

int main() {
  auto const n = read<ll>();
  auto const q = read<ll>();
  auto const nums = read_vec<ll>(n);
  std::vector<std::pair<ll, ll>> queries(q);
  for (auto &[k, v] : queries) {
    std::cin >> k >> v;
    --k;
  }
  solve(nums, queries);
}
