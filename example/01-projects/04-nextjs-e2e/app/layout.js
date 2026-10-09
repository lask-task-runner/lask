export const metadata = {
  title: "Lask todo",
  description: "A Next.js app whose build and browser tests are Lask tasks.",
};

export default function RootLayout({ children }) {
  return (
    <html lang="en">
      <body style={{ fontFamily: "system-ui, sans-serif", maxWidth: 480, margin: "3rem auto", padding: "0 1rem" }}>
        {children}
      </body>
    </html>
  );
}
