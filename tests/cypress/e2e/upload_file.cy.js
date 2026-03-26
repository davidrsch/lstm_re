describe("Upload file", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Upload Data");
  });

  it("'Upload file' CSV - upload button and file input are present", () => {
    cy.get('[data-testid="file"]').should('be.visible');
    cy.get('[data-testid="upload_file"] [type="file"]').should('not.be.visible');
  });

  it("'Upload file' CSV - after upload header/delimiter/decimal_point become enabled", () => {
    cy.upload_csv_flow();
    cy.get('[data-testid="header"]', { timeout: 10000 }).should('not.be.disabled');
    cy.get('[data-testid="delimiter"]', { timeout: 10000 }).should('not.be.disabled');
    cy.get('[data-testid="decimal_point"]', { timeout: 10000 }).should('not.be.disabled');
  });

  it("'Upload file' CSV - data analysis panel shows after upload", () => {
    cy.upload_csv_flow();
    cy.contains('Data Analysis').should('be.visible');
  });

  it("'Upload file' incorrect extension - warning modal appears", () => {
    cy.get('[data-testid="upload_file"] [type="file"]')
      .should('not.be.visible')
      .selectFile('cypress/fixtures/png_example.png', { force: true });
    cy.get('[role="dialog"]', { timeout: 10000 }).should('exist');
  });
});
