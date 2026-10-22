# HavNav

Indoor hospital navigation, directory, and operations software.

## Project structure

- `frontend/`: maintained HavNav React/TanStack Start application
- `backend/`: SSR wrappers, middleware, and server-side concerns
- `database/`: repository contracts and mock data adapters
- `shared/`: domain types, algorithms, and reusable services
- `legacy/`: original standalone DOM prototype retained for reference
- `requirements/`: requirements and architecture notes

The application currently uses in-memory data behind repository abstractions. The database boundary is ready for a persistent adapter without coupling storage to the frontend.

## Development setup

Requirements: Node.js and npm.

```bash
npm install
npm run dev
```

Useful checks:

```bash
npm run lint
npm run build
```

The project remains compatible with Lovable synchronization. Avoid rewriting published Git history.
