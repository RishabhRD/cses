#include <bits/stdc++.h>

using ll = long long;
constexpr ll mod = 1e9 + 7;

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

struct query_t {
  ll l;
  ll r;
  ll i;
};

struct query_ans_t {
  ll ans;
  ll i;
};

auto solve(std::vector<ll> const &nums,
           std::vector<std::pair<ll, ll>> const &queries_) {
  ll const n = std::size(nums);
  ll const qn = std::size(queries_);
  std::vector<query_t> queries;
  for (ll i = 0; i < qn; ++i) {
    queries.push_back({queries_[i].first, queries_[i].second, i});
  }
  std::sort(std::begin(queries), std::end(queries),
            [](auto a, auto b) { return a.r < b.r; });
  segment_tree stree(std::vector<ll>(n), std::plus<>{});
  ll j = 0;
  std::unordered_map<ll, ll> idx_map;
  std::vector<query_ans_t> res;
  for (ll i = 0; i < n; ++i) {
    auto const old_itr = idx_map.find(nums[i]);
    if (old_itr != std::end(idx_map)) {
      auto const old_idx = old_itr->second;
      stree.update(old_idx, [](auto a) { return 0; });
    }
    idx_map[nums[i]] = i;
    stree.update(i, [](auto a) { return 1; });
    while (j < qn && queries[j].r == i) {
      res.push_back({stree.query(queries[j].l, queries[j].r), queries[j].i});
      ++j;
    }
  }
  std::sort(std::begin(res), std::end(res),
            [](auto a, auto b) { return a.i < b.i; });
  for (auto [ans, _] : res) {
    std::cout << ans << std::endl;
  }
}

int main() {
  auto const n = read<ll>();
  auto const q = read<ll>();
  auto const nums = read_vec<ll>(n);
  std::vector<std::pair<ll, ll>> queries(q);
  for (auto &[a, b] : queries) {
    std::cin >> a >> b;
    --a;
    --b;
  }
  solve(nums, queries);
}
