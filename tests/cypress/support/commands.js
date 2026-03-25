// Disable CSS animations/transitions so FluentUI modals and panels are
// immediately visible in the headless Electron runner.
Cypress.on('window:load', (win) => {
  const style = win.document.createElement('style');
  style.textContent = `
    *, *::before, *::after {
      transition: none !important;
      animation: none !important;
      animation-duration: 0s !important;
      transition-duration: 0s !important;
    }
  `;
  win.document.head.appendChild(style);
});

// Upload a CSV file and wait for the data table to appear in the Data Analysis panel
Cypress.Commands.add('upload_csv_flow', () => {
  cy.get('[data-testid="file"]').should('be.visible');
  cy.get('[data-testid="upload_file"] [type="file"]')
    .should('not.be.visible')
    .selectFile('cypress/fixtures/csv_example.csv', { force: true });
  // Give Shiny time to process and render the data table
  cy.wait(3000);
});

// Navigate to a Pivot tab by its label text
Cypress.Commands.add('navigate_to_tab', (tabLabel) => {
  cy.contains('[role="tab"]', tabLabel).click({ force: true });
  cy.wait(1000);
});

// Click a card accordion toggle button identified by data-testid
Cypress.Commands.add('toggle_card', (testid) => {
  cy.get(`[data-testid="${testid}"]`).click({ force: true });
  cy.wait(500);
});

// Open a FluentUI multi-select Dropdown and click one or more option indices.
// calloutProps data-testid IS forwarded to the callout root div via
// FluentUI's getNativeProps divProperties whitelist (matches data-*).
Cypress.Commands.add('select_dropdown', (inputTestid, indices) => {
  cy.get(`[data-testid="${inputTestid}"]`).click({ force: true });
  const idxArray = Array.isArray(indices) ? indices : [indices];
  idxArray.forEach((index) => {
    cy.get('[role="listbox"]')
      .find(`[data-index="${index}"]`)
      .click({ force: true });
  });
  // Close callout by pressing Escape
  cy.get('body').type('{esc}');
});

// Assert that a Pivot tab with exact text is the currently selected tab.
// Uses anchored regex with cy.contains for full Cypress retryability.
// Timeout of 8 s to allow Shiny→runjs round-trip in CI.
Cypress.Commands.add('tab_should_be_active', (tabText) => {
  const regex = new RegExp('^' + tabText + '$');
  cy.contains('[role="tab"]', regex, { timeout: 8000 })
    .should('have.attr', 'aria-selected', 'true');
});
