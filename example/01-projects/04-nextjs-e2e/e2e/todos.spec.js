// @ts-check
const { test, expect } = require("@playwright/test");

test.beforeEach(async ({ page }) => {
  await page.goto("/");
});

test("starts empty", async ({ page }) => {
  await expect(page.getByRole("heading", { name: "Todos" })).toBeVisible();
  await expect(page.getByRole("listitem")).toHaveCount(0);
  await expect(page.getByRole("status")).toHaveText("0 items left");
});

test("adds, completes and deletes a todo", async ({ page }) => {
  const input = page.getByLabel("New todo");

  await input.fill("Write a Lask task");
  await input.press("Enter");
  await input.fill("Run it in CI");
  await page.getByRole("button", { name: "Add" }).click();

  await expect(page.getByRole("listitem")).toHaveText([/Write a Lask task/, /Run it in CI/]);
  await expect(page.getByRole("status")).toHaveText("2 items left");

  await page.getByRole("checkbox", { name: "Write a Lask task" }).check();
  await expect(page.getByRole("status")).toHaveText("1 item left");

  await page.getByRole("button", { name: "Delete Run it in CI" }).click();
  await expect(page.getByRole("listitem")).toHaveCount(1);
  await expect(page.getByRole("status")).toHaveText("0 items left");
});

test("ignores a blank todo", async ({ page }) => {
  await page.getByLabel("New todo").fill("   ");
  await page.getByRole("button", { name: "Add" }).click();
  await expect(page.getByRole("listitem")).toHaveCount(0);
});

test("health route answers", async ({ request }) => {
  const response = await request.get("/api/health");
  expect(response.ok()).toBeTruthy();
  expect(await response.json()).toEqual({ status: "ok" });
});
