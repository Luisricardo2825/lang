import "br.com.sankhya.extensions.actionbutton.AcaoRotinaJava";
import "br.com.sankhya.extensions.actionbutton.ContextoAcao";
import "br.com.sankhya.jape.EntityFacade";
import "br.com.sankhya.jape.dao.JdbcWrapper";
import "br.com.sankhya.modelcore.MGEModelException";
import "br.com.sankhya.modelcore.util.EntityFacadeFactory";
import "br.com.sankhya.ws.ServiceContext";
import "com.google.gson.JsonArray";
import "com.google.gson.JsonElement";
import "com.google.gson.JsonObject";
import "com.sankhya.util.JdbcUtils";
import "org.jetbrains.annotations.NotNull";

import "java.sql.PreparedStatement";
import "java.sql.ResultSet";
import "java.sql.SQLException";
import "java.util.AbstractMap";
import "java.util.Map";

const ctx = ServiceContext.getCurrent();
const request = ctx.getJsonRequestBody();
try {
  const sql = request.get("sql").getAsString();
  const response = execute(sql);
  ServiceContext.getCurrent().setJsonResponse(response);
} catch (e) {
  throw new Exception(e.getMessage());
}

function execute(sql) {
  const response = new JsonObject();
  const jdbc = null;
  let rset = null;
  const start = System.currentTimeMillis();
  try {
    const entity = EntityFacadeFactory.getDWFFacade();
    jdbc = entity.getJdbcWrapper();
    jdbc.openSession();
    const upd = jdbc.getPreparedStatement(query);
    const executeStatus = upd.execute();
    let rowsUpdated = upd.getUpdateCount();
    rset = upd.getResultSet();
    const end = start - System.currentTimeMillis();
    if (executeStatus) {
      const rows = GetResults(rset);
      response.add("rows", rows.getKey());
      rowsUpdated = rows.getValue();
    }
    response.addProperty("rowsUpdated", rowsUpdated);
    response.addProperty("executeStatus", executeStatus);
    response.addProperty("queryTime", Math.abs(end));
    return response;
  } catch (e) {
    MGEModelException.throwMe(e);
  } finally {
    JdbcUtils.closeResultSet(rset);
    JdbcWrapper.closeSession(jdbc);
  }
  return null;
}

function GetResults(rset) {
  const results = new JsonArray();
  const total_cols = rset.getMetaData().getColumnCount();
  let rowsUpdated = 0;

  while (rset.next()) {
    rowsUpdated++;
    const colJson = new JsonObject();
    for (let col = 1; col <= total_cols; col++) {
      const value = rset.getString(col);
      const colLabel = rset.getMetaData().getColumnLabel(col);
      if (!colJson.has(colLabel)) {
        colJson.addProperty(colLabel, value);
      } else {
        colJson.addProperty(colLabel + col, value); // Caso seja um campo repetido
      }
    }
    results.add(colJson);
  }
  return new AbstractMap.SimpleEntry(results, rowsUpdated);
}
