<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    xmlns:sos="http://www.opengis.net/sos/2.0" 
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
    xmlns:swes="http://www.opengis.net/swes/2.0" 
    xmlns:gml="http://www.opengis.net/gml/3.2" version="1.0">
    <!-- Created By: Alessandro Oggioni - CNR IREA in Milano - 2019-10-25T22:30:00Z -->
    <!-- Licence CC By SA 3.0  http://creativecommon.org/licences/by-SA/4.0 -->
    <xsl:output method="xml" version="1.0" encoding="UTF-8" omit-xml-declaration="yes" indent="yes"
        media-type="text/xml"/>
    <xsl:output method="text" encoding="utf-8" />
    <xsl:variable name="separator" select="'&#59;'" />
    <xsl:variable name="newline" select="'&#10;'" />
    
    <xsl:template match="/">
        <!-- Inizio output -->
        <xsl:text>uri</xsl:text>
        <xsl:value-of select="$newline" />
        <xsl:for-each select="//sos:Capabilities/sos:contents/sos:Contents/swes:offering/sos:ObservationOffering/swes:procedure">
            <xsl:value-of select="." />
            <xsl:value-of select="$newline" />
        </xsl:for-each>
    </xsl:template>
</xsl:stylesheet>