describe("Training vectors options card", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Selecting Features");
    cy.wait(2000);
    // Open training vectors card
    cy.toggle_card("toggle_tv_card");
    cy.wait(500);
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
    cy.wait(1000);
    // Error modal should appear
    cy.get('[role="dialog"]').should('exist');
    cy.contains('Temporal horizon must be an integer number bigger than 0').should('exist');
  });

  it("Card starts hidden before being opened", () => {
    cy.visit("/");
    cy.navigate_to_tab("Selecting Features");
    cy.wait(1000);
    cy.get('[data-testid="temporalhorizon"]').should('not.be.visible');
  });
});
