describe("Training vectors options card", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Selecting Features");
    // Wait for the server to render before opening the card
    cy.get('[data-testid="selectimeseries"]', { timeout: 10000 }).should('exist');
    // Open training vectors card and wait for content to become visible
    cy.toggle_card("toggle_tv_card");
    cy.get('[data-testid="temporalhorizon"]', { timeout: 8000 }).should('be.visible');
  });

  it("Temporal horizon field is visible after opening card", () => {
    cy.get('[data-testid="temporalhorizon"]').should('be.visible');
  });

  it("Temporal horizon defaults to a numeric value", () => {
    cy.get('[data-testid="temporalhorizon"] input').should('have.attr', 'type', 'number');
  });

  it("Temporal horizon must be a positive integer - shows error for invalid value", () => {
    cy.get('[data-testid="temporalhorizon"] input')
      .clear({ force: true })
      .type('0', { force: true })
      .then(($el) => {
        // Use native blur() rather than cy.blur() so the synchronous shiny.react
        // v0.1.0 querySelector error (which fires through Cypress's $Cypress.pause
        // execution context and bypasses uncaught:exception) is caught here.
        try { $el[0].blur(); } catch (e) { /* shiny.react v0.1.0 internal - safe to ignore */ }
      });
    cy.get('[role="dialog"]', { timeout: 10000 }).should('exist');
    cy.contains('Temporal horizon must be an integer number bigger than 0').should('exist');
  });

  it("Card starts hidden before being opened", () => {
    cy.visit("/");
    cy.navigate_to_tab("Selecting Features");
    cy.get('[data-testid="selectimeseries"]', { timeout: 10000 }).should('exist');
    cy.get('[data-testid="temporalhorizon"]', { timeout: 8000 }).should('not.be.visible');
  });
});
