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

// Open a FluentUI multi-select Dropdown and click one or more option indices
Cypress.Commands.add('select_dropdown', (inputTestid, indices) => {
  cy.get(`[data-testid="${inputTestid}"]`).click({ force: true });
  const idxArray = Array.isArray(indices) ? indices : [indices];
  idxArray.forEach((index) => {
    cy.get(`[data-testid="${inputTestid}-callout"]`)
      .find(`[data-index="${index}"]`)
      .click({ force: true });
  });
  // Close callout by pressing Escape
  cy.get('body').type('{esc}');
});
