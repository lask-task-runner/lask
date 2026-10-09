"use client";

import { useState } from "react";

// A todo list kept in the browser: nothing is saved, so every test
// starts from an empty list.
export default function Todos() {
  const [items, setItems] = useState([]);
  const [text, setText] = useState("");

  function add(event) {
    event.preventDefault();
    const title = text.trim();
    if (!title) return;
    setItems([...items, { id: crypto.randomUUID(), title, done: false }]);
    setText("");
  }

  function toggle(id) {
    setItems(items.map((item) => (item.id === id ? { ...item, done: !item.done } : item)));
  }

  function remove(id) {
    setItems(items.filter((item) => item.id !== id));
  }

  const left = items.filter((item) => !item.done).length;

  return (
    <>
      <form onSubmit={add}>
        <input
          aria-label="New todo"
          placeholder="What needs doing?"
          value={text}
          onChange={(event) => setText(event.target.value)}
        />{" "}
        <button type="submit">Add</button>
      </form>

      <ul>
        {items.map((item) => (
          <li key={item.id}>
            <label style={{ textDecoration: item.done ? "line-through" : "none" }}>
              <input type="checkbox" checked={item.done} onChange={() => toggle(item.id)} /> {item.title}
            </label>{" "}
            <button aria-label={`Delete ${item.title}`} onClick={() => remove(item.id)}>
              ×
            </button>
          </li>
        ))}
      </ul>

      <p role="status">{left === 1 ? "1 item left" : `${left} items left`}</p>
    </>
  );
}
