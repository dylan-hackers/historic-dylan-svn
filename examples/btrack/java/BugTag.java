package sigue.btrack;

import javax.servlet.jsp.*;
import javax.servlet.jsp.tagext.*;
import javax.servlet.http.HttpSession;
import java.io.IOException;

public class BugTag extends RecordTag
{
    protected void doRecordTag (DatabaseRecord rec,
                                JspWriter out,
                                HttpSession session)
        throws JspException, IOException
    {
        Bug record = (Bug) rec;
        // What's less disgusting, this or having a whole new class
        // for each tag?  My guess is that this is less error-prone.
        if ("priority".equalsIgnoreCase(key)) {
            showPriorityOptions(out, record);
        } else if ("severity".equalsIgnoreCase(key)) {
            showSeverityOptions(out, record);
        } else if ("version".equalsIgnoreCase(key)) {
            showVersionOptions(out, record);
        } else if ("operating_system".equalsIgnoreCase(key)) {
            showOperatingSystemOptions(out, record);
        } else if ("platform".equalsIgnoreCase(key)) {
            showPlatformOptions(out, record);
        } else if ("browser".equalsIgnoreCase(key)) {
            showBrowserOptions(out, record);
        } else if ("module".equalsIgnoreCase(key)) {
            showModuleOptions(out, record);
        } else if ("product".equalsIgnoreCase(key)) {
            showProductOptions(out, record);
        } else if ("dev_assigned".equalsIgnoreCase(key)) {
            showDevAssignedOptions(out, record);
        } else if ("qa_assigned".equalsIgnoreCase(key)) {
            showQaAssignedOptions(out, record);
        } else if ("status".equalsIgnoreCase(key)) {
            showStatusOptions(out, record);
        } else {
            Debug.println("BugTag.doTag: unrecognized key: " + key);
        }
    }

    private void showPriorityOptions (JspWriter out, Bug record) {
        showNumberedOptions(out, Util.getPriorityOptions(), record.getPriority().intValue());
    }

    private void showSeverityOptions (JspWriter out, Bug record) {
        showNumberedOptions(out, Util.getSeverityOptions(), record.getSeverity().intValue());
    }

    private void showVersionOptions (JspWriter out, Bug record) {
        showNumberedOptions(out,
                            Util.getVersionOptions(),
                            record.getVersion().getId().intValue());
    }

    private void showOperatingSystemOptions (JspWriter out, Bug record) {
        showNumberedOptions(out,
                            Util.getOperatingSystemOptions(),
                            record.getOperatingSystem().getId().intValue());
    }

    private void showPlatformOptions (JspWriter out, Bug record) {
        showNumberedOptions(out,
                            Util.getPlatformOptions(),
                            record.getPlatform().getId().intValue());
    }

    private void showBrowserOptions (JspWriter out, Bug record) {
        showNumberedOptions(out,
                            Util.getBrowserOptions(),
                            record.getBrowser().getId().intValue());
    }

    private void showStatusOptions (JspWriter out, Bug record) {
        showNumberedOptions(out,
                            Util.getStatusOptions(),
                            record.getStatus().intValue());
    }

    private void showModuleOptions (JspWriter out, Bug record) {
        showNumberedOptions(out,
                            Util.getModuleOptions(),
                            record.getModule().getId().intValue());
    }

    private void showProductOptions (JspWriter out, Bug record) {
        showNumberedOptions(out,
                            Util.getProductOptions(),
                            record.getProduct().getId().intValue());
    }

    //---TODO: Only display developer accounts?
    private void showDevAssignedOptions (JspWriter out, Bug record) {
        showNumberedOptions(out,
                            Util.getAccountOptions(),
                            record.getDevAssigned().getId().intValue());
    }

    private void showQaAssignedOptions (JspWriter out, Bug record) {
        showNumberedOptions(out,
                            Util.getAccountOptions(),
                            record.getQaAssigned().getId().intValue());
    }

}
