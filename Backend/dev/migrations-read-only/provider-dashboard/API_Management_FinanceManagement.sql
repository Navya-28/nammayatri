-- {"api":"PostFinanceManagementFinanceAdjustmentSubmit","migration":"capability","param":"finance.adjustment.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.adjustment.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_FINANCE_ADJUSTMENT_SUBMIT' ) ON CONFLICT DO NOTHING;

-- {"api":"GetFinanceManagementFinanceAdjustmentList","migration":"capability","param":"finance.adjustment.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.adjustment.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_ADJUSTMENT_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFinanceManagementFinanceAdjustmentApprove","migration":"capability","param":"finance.adjustment.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.adjustment.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_FINANCE_ADJUSTMENT_APPROVE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostFinanceManagementFinanceAdjustmentReject","migration":"capability","param":"finance.adjustment.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.adjustment.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_FINANCE_ADJUSTMENT_REJECT' ) ON CONFLICT DO NOTHING;
