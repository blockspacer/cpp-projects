# Intrusive List


В данном задании необходимо написать [Intrusive List](https://www.boost.org/doc/libs/1_74_0/doc/html/intrusive/intrusive_vs_nontrusive.html). Intrusive List —  это контейнер, который представляет из себя двусвязный список. В отличие от `std::list` он не делает копии элементов вставляемых в список, а строит список из тех объектов, что ему передаются в `insert`, `push_front` и `push_back`. 

## Пример использования
```cpp
struct bookmark : intrusive::list_element<struct bookmark_tag>
{
    uint32_t page;

    bookmark(uint32_t page)
        : page(page)
    {}
};

int main()
{
    std::vector<bookmark> bookmarks;
    for (uint32_t i = 0; i != 100; i += 10)
        bookmarks.push_back(bookmark(i));

    intrusive::list<bookmark, bookmark_tag> bookmark_list;
    for (auto& e : bookmarks)
        bookmark_list.push_front(e);

    for (auto const& e : bookmark_list)
        std::cout << e.get_page() << '\n';
}
```
В `intrusive::list` можно добавлять объекты не любых типов, а лишь тех, что наследуются от `intrusive::list_element<Tag>`. Шаблонный параметр `Tag` позволяет пользовательскому классу отнаследоваться сразу несколько раз от `intrusive::list_element` если это необходимо. Множественное наследование от `intrusive::list_element<Tag>` необходимо, если нужно добавлять один объект сразу в несколько разных списков. Чтобы указать `intrusive::list<T, Tag>` какую из баз у `T` он использует, ему передаётся `Tag` вторым параметром.

Если у `list_element` вызывается деструктор и этот `list_element` добавлен в какой-то список, то он должен быть автоматически удалён из этого списка (в `Boost.Intrusive` такое поведение называется [auto-unlink hooks](https://www.boost.org/doc/libs/1_74_0/doc/html/intrusive/auto_unlink_hooks.html)).
