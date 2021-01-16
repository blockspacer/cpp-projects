#pragma once

#include <cstddef>
#include <functional>
#include <utility>
#include <cstdlib>
#include <type_traits>
#include <stdexcept>

template <typename Left, typename Right, typename CompareLeft = std::less<Left>,
          typename CompareRight = std::less<Right>>
struct bimap {
private:
  std::size_t size_ = 0;
public:
  using left_t = Left;
  using right_t = Right;
  struct left_tag;
  struct right_tag;


  template <typename Tag, typename T>
  struct node_s {
    node_s* left = nullptr;
    node_s* right = nullptr;
    node_s* parent = nullptr;
    int prior = rand();
    //T key;

    const T& key() {
      return static_cast<node_t*>(this)->template key_f<Tag>();
    }

    node_s() = default;

    node_s* next() noexcept {
      node_s* ptr = this;
      if (ptr->right != nullptr) {
        ptr = ptr->right;
        while (ptr->left != nullptr)
          ptr = ptr->left;
      } else {
        auto y = ptr->parent;
        while (y != nullptr && ptr == y->right) {
          ptr = y;
          y = y->parent;
        }
        ptr = y;
      }
      return ptr;
    }

    node_s* prev() noexcept{
      node_s* ptr = this;
      if (ptr->left != nullptr) {
        ptr = ptr->left;
        while (ptr->right != nullptr)
          ptr = ptr->right;
      } else {
        auto y = ptr->parent;
        while (y != nullptr && ptr == y->left) {
          ptr = y;
          y = y->parent;
        }
        ptr = y;
      }
      return ptr;
    }
  };

  using left_node = node_s<left_tag, left_t>;
  using right_node = node_s<right_tag, right_t>;

  struct node_t : left_node, right_node {
    //node_t() = default;
    left_t left;
    right_t right;

    template <class tag>
    const std::conditional_t<std::is_same_v<tag, left_tag>, left_t, right_t>& key_f() {
      if constexpr (std::is_same_v<tag, left_tag>) {
        return left;
      } else {
        return right;
      }
    }
    node_t(left_t a, right_t b) : left(std::move(a)), right(std::move(b)){}
  };


  template <typename Tag, typename T, typename Comparator>
  struct tree {
    using node_ptr = node_s<Tag, T>*;
    Comparator cmp;

    mutable node_s<Tag, T> fake_;
    node_ptr fake = &fake_;

    explicit tree(Comparator compare = Comparator()) : cmp(compare) {}

    tree(node_ptr fake, tree&& t) noexcept {
      std::swap(fake->left, t.fake->left);
    }

    bool key_equal(const T& a, const T& b) const {
      return !cmp(a, b) && !cmp(b, a);
    }

    bool key_less(const T& a, const T& b) const {
      return cmp(a, b);
    }

    node_ptr find(node_ptr t, const T& x) const {
      if (t == nullptr) {
        return end();
      }

      if (key_less(x, t->key())) {
        return find(t->left, x);
      } else if (key_less(t->key(), x)) {
        return find(t->right, x);
      } else {
        return t;
      }
    }

    node_ptr find(const T& x) const {
      return find(fake->left, x);
    }

    node_ptr lower_bound (const T& x) const { // >=
      node_ptr current = fake->left;
      node_ptr res = nullptr;
      while (current != nullptr) {
        if (!key_less(current->key(), x)) {
          res = current;
          current = current->left;
        } else {
          current = current->right;
        }
      }
      if (res == nullptr)
        return fake;
      return res;
    }

    node_ptr upper_bound (const T& x) const { // >
      node_ptr current = fake->left;
      node_ptr res = nullptr;
      while (current != nullptr) {
        if (key_less(x, current->key())) {
          res = current;
          current = current->left;
        } else {
          current = current->right;
        }
      }
      if (res == nullptr)
        return fake;
      return res;
    }

    void update_parents(node_ptr x) {
      if (x != nullptr) {
        //x->parent = nullptr;
        if (x->left != nullptr) {
          x->left->parent = x;
        }
        if (x->right != nullptr) {
          x->right->parent = x;
        }
      }
    }

    void split(node_ptr t, const T& key, node_ptr & l, node_ptr & r) {
      if (t == nullptr) {
        l = nullptr;
        r = nullptr;
      } else if (key_less(key, t->key())) {
        split(t->left, key, l, t->left);
        r = t;
      } else {
        split(t->right, key, t->right, r);
        l = t;
      }
      update_parents(l);
      update_parents(r);
    }

    void merge(node_ptr& t, node_ptr l, node_ptr r) {
      if (l == nullptr || r == nullptr) {
        t = l == nullptr ? r : l;
      } else if (l->prior > r->prior) {
        merge(l->right,l->right, r);
        update_parents(l);
        update_parents(r);
        t = l;
      } else {
        merge(r->left,l, r->left);
        update_parents(l);
        update_parents(r);
        t = r;
      }
    }

    node_ptr insert(node_ptr x) {
      auto res = insert(fake->left, x);
      update_parents(fake->left);
      update_parents(fake);
      return res;
    }

    node_ptr insert(node_ptr &t, node_ptr it) {
      if (t == nullptr) {
        t = it;
      } else if (it->prior > t->prior) {
        split(t, it->key(), it->left, it->right);
        t = it;
      } else {
        if (key_less(it->key(), t->key())) {
          insert(t->left, it);
        } else {
          insert(t->right, it);
        }
      }
      update_parents(t);
      return it;
    }

    node_ptr erase(node_ptr& x) {
      if (x == nullptr)
        return fake;
      auto res = x->next();
      auto parent = x->parent;
      merge(x == parent->left ? parent->left : parent->right, x->left, x->right);
      update_parents(parent);

      return res;
    }

    node_ptr begin() const noexcept {
      auto t = fake;

      while (t->left != nullptr) {
        t = t->left;
      }
      return t;
    }

    node_ptr end() const noexcept {
      return fake;
    }

    ~tree() = default;
  };

  template <typename Tag, typename T>
  struct iterator {
    node_s<Tag, T>* ptr = nullptr;

    iterator() = default;

    iterator(node_s<Tag, T>* node) : ptr(node) {}

    T const& operator*() const noexcept{
      return ptr->key();
    }

    iterator &operator++() noexcept {
      ptr = ptr->next();
      return *this;
    }

    iterator operator++(int) noexcept {
      const auto return_value = *this;
      ptr = ptr->next();
      return return_value;
    }

    iterator &operator--() noexcept {
      ptr = ptr->prev();
      return *this;
    }

    iterator operator--(int) noexcept {
      const auto return_value = *this;
      ptr = ptr->prev();
      return *return_value;
    }

    bool operator==(const iterator& other) const noexcept {
      return ptr == other.ptr;
    }

    bool operator!=(const iterator& other) const noexcept {
      return ptr != other.ptr;
    }

    std::conditional_t<std::is_same_v<Tag, left_tag>, iterator<right_tag, right_t>, iterator<left_tag, left_t>>
        flip() const noexcept {
      if constexpr (std::is_same_v<Tag, left_tag>)
        return static_cast<right_node*>(static_cast<node_t*>(ptr));
      else
        return static_cast<left_node*>(static_cast<node_t*>(ptr));
    }
  };


  using right_iterator = iterator<right_tag, right_t>;
  using left_iterator = iterator<left_tag, left_t>;


  // Создает bimap не содержащий ни одной пары.
  explicit bimap(CompareLeft compare_left = CompareLeft(),
        CompareRight compare_right = CompareRight()) : left_tree(compare_left), right_tree(compare_right) {

  }

  // Конструкторы от других и присваивания
  bimap(bimap const &other) : left_tree(other.left_tree.cmp), right_tree(other.right_tree.cmp) {
    auto it = other.begin_left();
    try {
      for (; it != other.end_left(); ++it) {
        insert(*it, *(it.flip()));
      }
    } catch(...) {
      clear();
      throw;
    }
  }

  bimap(bimap &&other) noexcept : left_tree(std::move(other.left_tree)),
                                  right_tree(std::move(other.right_tree)),
                                  size_(std::move(other.size_)){}

  bimap &operator=(bimap const &other) {
    clear();
    auto it = other.begin_left();
    for (; it != other.end_left(); ++it) {
      insert(*it, *(it.flip()));
    }
    return *this;
  }

  bimap &operator=(bimap &&other) {
    clear();
    size_ = std::move(other.size());
    left_tree = std::move(other.left_tree);
    right_tree = std::move(other.right_tree);
    return *this;
  }

  void clear() {
    auto i = begin_left();
    while(!empty()) {
      auto next = i;
      ++next;
      erase_left(i);
      i = next;
    }
  }
  // Деструктор. Вызывается при удалении объектов bimap.
  // Инвалидирует все итераторы ссылающиеся на элементы этого bimap
  // (включая итераторы ссылающиеся на элементы следующие за последними).
  ~bimap() {
    clear();
  }

  left_iterator exception_safe_insert(node_t* elem) {
    try {
      auto res = insert(elem);
      return res;
    } catch (...) {
      delete elem;
      throw;
    }
  }

  // Вставка пары (left, right), возвращает итератор на left.
  // Если такой left или такой right уже присутствуют в bimap, вставка не
  // производится и возвращается end_left().
  left_iterator insert(left_t const &left, right_t const &right) {
    auto elem = new node_t(left, right);
    return exception_safe_insert(elem);
  }

  left_iterator insert(left_t const &left, right_t &&right) {
    auto elem = new node_t(left, std::move(right));
    return exception_safe_insert(elem);
  }

  left_iterator insert(left_t &&left, right_t const &right) {
    auto elem = new node_t(std::move(left), right);
    return exception_safe_insert(elem);
  }

  left_iterator insert(left_t &&left, right_t &&right) {
    auto elem = new node_t(std::move(left), std::move(right));
    return exception_safe_insert(elem);
  }

  // Удаляет элемент и соответствующий ему парный.
  // erase невалидного итератора неопределен.
  // erase(end_left()) и erase(end_right()) неопределены.
  // Пусть it ссылается на некоторый элемент e.
  // erase инвалидирует все итераторы ссылающиеся на e и на элемент парный к e.
  left_iterator erase_left(left_iterator it) {
    return erase(it);
  }
  // Аналогично erase, но по ключу, удаляет элемент если он присутствует, иначе
  // не делает ничего Возвращает была ли пара удалена
  bool erase_left(left_t const &left) {
    return erase_bool(left);
  }

  right_iterator erase_right(right_iterator it) {
    return erase(it);
  }

  bool erase_right(right_t const &right) {
    return erase_bool(right);
  }

  // erase от ренжа, удаляет [first, last), возвращает итератор на последний
  // элемент за удаленной последовательностью
  left_iterator erase_left(left_iterator first, left_iterator last) {
    return range_erase(first, last);
  }

  right_iterator erase_right(right_iterator first, right_iterator last) {
    return range_erase(first, last);
  }

  // Возвращает итератор по элементу. Если не найден - соответствующий end()
  left_iterator find_left(left_t const &left) const {
    return left_tree.find(left);
  }
  right_iterator find_right(right_t const &right) const {
    return right_tree.find(right);
  }

  // Возвращает противоположный элемент по элементу
  // Если элемента не существует -- бросает std::out_of_range
  right_t const &at_left(left_t const &key) const {
    return at_side<true>(key);
  }

  left_t const &at_right(right_t const &key) const {
    return at_side<false>(key);
  }
  // Возвращает противоположный элемент по элементу
  // Если элемента не существует, добавляет его в bimap и на противоположную
  // сторону кладет дефолтный элемент, ссылку на который и возвращает
  // Если дефолтный элемент уже лежит в противоположной паре - должен поменять
  // соответствующий ему элемент на запрашиваемый (смотри тесты)
  template <typename T = right_t, typename = std::enable_if_t<std::is_default_constructible_v<T>>>
  right_t const &at_left_or_default(left_t const &key) {
    auto lit = find_left(key);
    auto rit = find_right(right_t{});
    if (lit != end_left()) {
      return *lit.flip();
    }

    if (rit != end_right()) {
      erase_right(rit);
      auto res = insert(key, right_t{});
      return *res.flip();
    }
    return *(insert(key, right_t{}).flip());
  }

  template <typename T = left_t, typename = std::enable_if_t<std::is_default_constructible_v<T>>>
  left_t const &at_right_or_default(right_t const &key) {
    auto lit = find_right(key);
    auto rit = find_left(left_t{});
    if (lit != end_right()) {
      return *lit.flip();
    }

    if (rit != end_left()) {
      erase_left(rit);
      auto res = insert(left_t{}, key);
      return *res;
    }
    return *(insert(left_t{}, key));
  }

  // lower и upper bound'ы по каждой стороне
  // Возвращают итераторы на соответствующие элементы
  // Смотри std::lower_bound, std::upper_bound.
  left_iterator lower_bound_left(const left_t &left) const {
    return left_tree.lower_bound(left);
  }

  left_iterator upper_bound_left(const left_t &left) const {
    return left_tree.upper_bound(left);
  }

  right_iterator lower_bound_right(const right_t &left) const {
    return right_tree.lower_bound(left);
  }

  right_iterator upper_bound_right(const right_t &left) const {
    return right_tree.upper_bound(left);
  }

  // Возващает итератор на минимальный по порядку left.
  left_iterator begin_left() const noexcept {
    return left_tree.begin();
  }
  // Возващает итератор на следующий за последним по порядку left.
  left_iterator end_left() const noexcept {
    return static_cast<left_node*>(&left_tree.fake_);
  }

  // Возващает итератор на минимальный по порядку right.
  right_iterator begin_right() const noexcept {
    return right_tree.begin();
  }
  // Возващает итератор на следующий за последним по порядку right.
  right_iterator end_right() const noexcept {
    return static_cast<right_node*>(&right_tree.fake_);
  }

  // Проверка на пустоту
  [[nodiscard]] bool empty() const noexcept {
    return size_ == 0;
  }

  // Возвращает размер бимапы (кол-во пар)
  [[nodiscard]] std::size_t size() const noexcept {
    return size_;
  }

  // операторы сравнения
  template <typename L, typename R, typename CL, typename CR>
  friend bool operator==(bimap const &a, bimap const &b);

  template <typename L, typename R, typename CL, typename CR>
  friend bool operator!=(bimap const &a, bimap const &b);

private:
  tree<right_tag, right_t, CompareRight> right_tree;
  tree<left_tag, left_t, CompareLeft> left_tree;

private:
  template<bool is_left, typename Type>
  std::conditional_t<is_left, right_t, left_t> const& at_side(Type const& key) const {
    std::conditional_t<is_left, left_iterator, right_iterator> it;
    if constexpr (is_left) {
      it = find_left(key);
      if (it.ptr == &left_tree.fake_) {
        throw std::out_of_range("can't find element");
      }
    } else {
      it = find_right(key);
      if (it.ptr == &right_tree.fake_) {
        throw std::out_of_range("can't find element");
      }
    }

    return *(it.flip());
  }

  template <typename Iter>
  Iter range_erase(Iter first, Iter last) {
    auto res = first;
    auto it = first;
    while (it != last) {
      auto next = it;
      next++;
      if constexpr (std::is_same_v<Iter, left_iterator>) {
        res = erase_left(it);
      } else {
        res = erase_right(it);
      }
      it = next;
    }
    return res;
  }

  left_iterator insert(node_t* node) {
    if (is_here(static_cast<left_node*>(node)->key(), static_cast<right_node*>(node)->key())) {
      delete node;
      return end_left();
    }
    auto res = left_tree.insert(static_cast<left_node*>(node));
    right_tree.insert(static_cast<right_node*>(node));
    size_++;
    return res;
  }

  template<typename K>
  bool erase_bool(K const &k) {
    constexpr bool is_left = std::is_same_v<K, left_t>;
    std::conditional_t<is_left, left_iterator, right_iterator> it;
    if constexpr (is_left) {
      it = left_tree.find(k);
      if (it != left_tree.fake) {
        erase_left(it);
        return true;
      } else {
        return false;
      }
    } else {
      it = right_tree.find(k);
      if (it != right_tree.fake) {
        erase_right(it);
        return true;
      } else {
        return false;
      }
    }

    return false;
  }

  template<typename Iter>
  Iter erase(Iter it) {
    auto rit = it.flip();
    Iter res;
    if constexpr (std::is_same_v<Iter, left_iterator>) {
      res = left_tree.erase(it.ptr);
      right_tree.erase(rit.ptr);
    } else {
      res = right_tree.erase(it.ptr);
      left_tree.erase(rit.ptr);
    }
    delete static_cast<node_t*>(it.ptr);
    size_--;
    return res;
  }

  bool is_here(left_t const& left, right_t const& right) {
    return find_left(left) != end_left() || find_right(right) != end_right();
  }
};

template <typename L, typename R, typename CL, typename CR>
bool operator==(bimap<L, R, CL, CR> const &a, bimap<L, R, CL, CR> const &b) {
  if (a.size() != b.size()) {
    return false;
  }
  auto lit = b.begin_left();
  auto lit1 = a.begin_left();
  for(; lit1 != a.end_left(), lit != b.end_left(); ++lit1, ++lit) {
    if (*lit != *lit1)
      return false;
  }

  auto rit = b.begin_right();
  auto rit1 = a.begin_right();
  for(; rit1 != a.end_right(), rit != b.end_right(); ++rit1, ++rit) {
    if (*rit != *rit1)
      return false;
  }

  return true;
}

template <typename L, typename R, typename CL, typename CR>
bool operator!=(bimap<L, R, CL, CR> const &a, bimap<L, R, CL, CR> const &b) {
  return !(a == b);
}
