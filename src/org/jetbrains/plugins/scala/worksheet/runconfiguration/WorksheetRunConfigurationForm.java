package org.jetbrains.plugins.scala.worksheet.runconfiguration;


import com.intellij.execution.ui.ClassBrowser;
import com.intellij.execution.ui.ConfigurationModuleSelector;
import com.intellij.ide.util.ClassFilter;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiClass;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.ui.RawCommandLineEditor;
import org.jetbrains.plugins.scala.ScalaFileType;
import org.jetbrains.plugins.scala.ScalaFileTypeFactory;
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil;
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager;

import javax.swing.*;

/**
 * @author Ksenia.Sautina
 * @since 10/16/12
 */
public class WorksheetRunConfigurationForm {
  private RawCommandLineEditor javaOptionsEditor;
  private JPanel myPanel;
  private TextFieldWithBrowseButton workingDirectoryField;
  private TextFieldWithBrowseButton worksheetField;
  private JComboBox moduleComboBox;
  private Project myProject;
  private WorksheetRunConfiguration myConfiguration;

  private ConfigurationModuleSelector myModuleSelector;
  public WorksheetRunConfigurationForm(final Project project,
                                          final WorksheetRunConfiguration configuration) {
    myModuleSelector = new ConfigurationModuleSelector(project, moduleComboBox);
    myModuleSelector.reset(configuration);
    moduleComboBox.setEnabled(true);
    myProject = project;
    myConfiguration = configuration;
    javaOptionsEditor.setName("VM options");
    javaOptionsEditor.setDialogCaption("VM options editor");
    javaOptionsEditor.setText("-Djline.terminal=NONE");
    addFileChooser("Choose Working Directory", workingDirectoryField, project);
    addWSChooser("Choose Worksheet", worksheetField, project);
    VirtualFile baseDir = project.getBaseDir();
    String path = baseDir != null ? baseDir.getPath() : "";
    workingDirectoryField.setText(path);
  }

  public JPanel getPanel() {
    return myPanel;
  }

  public String getJavaOptions() {
    return javaOptionsEditor.getText();
  }

  public void setJavaOptions(String s) {
    javaOptionsEditor.setText(s);
  }

  public void apply(WorksheetRunConfiguration configuration) {
    setJavaOptions(configuration.getJavaOptions());
    setWorkingDirectory(configuration.getWorkingDirectory());
    setWorksheetField(configuration.getWorksheetField());
    myModuleSelector.applyTo(configuration);
  }

  public String getWorkingDirectory() {
    return workingDirectoryField.getText();
  }

  public void setWorkingDirectory(String s) {
    workingDirectoryField.setText(s);
  }


  public String getWorksheetField() {
    return worksheetField.getText();
  }

  public void setWorksheetField(String s) {
    worksheetField.setText(s);
  }

  public Module getModule() {
    return myModuleSelector.getModule();
  }

  private FileChooserDescriptor addFileChooser(final String title,
                                               final TextFieldWithBrowseButton textField,
                                               final Project project) {
    final FileChooserDescriptor fileChooserDescriptor = new FileChooserDescriptor(false, true, false, false, false, false) {
      @Override
      public boolean isFileVisible(VirtualFile file, boolean showHiddenFiles) {
        return super.isFileVisible(file, showHiddenFiles) && file.isDirectory();
      }
    };
    fileChooserDescriptor.setTitle(title);
    textField.addBrowseFolderListener(title, null, project, fileChooserDescriptor);
    return fileChooserDescriptor;
  }


  private FileChooserDescriptor addWSChooser(final String title,
                                               final TextFieldWithBrowseButton textField,
                                               final Project project) {
    final FileChooserDescriptor fileChooserDescriptor = new FileChooserDescriptor(true, false, false, false, false, false) {
      @Override
      public boolean isFileVisible(VirtualFile file, boolean showHiddenFiles) {
        //TODO worksheet file type
        return super.isFileVisible(file, showHiddenFiles) && (file.isDirectory() || ScalaFileType.WORKSHEET_EXTENSION.equals(file.getExtension()));
      }
    };
    fileChooserDescriptor.setTitle(title);
    textField.addBrowseFolderListener(title, null, project, fileChooserDescriptor);
    return fileChooserDescriptor;
  }
}
