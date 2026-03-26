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
      .blur();
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
